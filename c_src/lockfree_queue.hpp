// -------------------------------------------------------------------
//
// Copyright (c) 2007-2012 Basho Technologies, Inc. All Rights Reserved.
//
// This file is provided to you under the Apache License,
// Version 2.0 (the "License"); you may not use this file
// except in compliance with the License.  You may obtain
// a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.
//
// -------------------------------------------------------------------
#ifndef LOCKFREE_QUEUE_HPP_
#define LOCKFREE_QUEUE_HPP_


#include <cstddef>
#include <cstdio>
#include <boost/atomic.hpp>

//// specialize this for queue item types to calculate total byte size of queue
template <typename T> std::size_t item_size(T item) { return sizeof(item); }

template <typename T>
class lockfree_queue
{
    typedef T item_type;
    typedef std::size_t index_type;
    
    struct slot
    {
        item_type item;
        boost::atomic_int used;
    };
    char pad0_[64];
    std::size_t size_;
    slot* const storage_;
    std::size_t read_;
    char pad1_[64];
    std::size_t write_;
    char pad2_[64];
    boost::atomic<size_t> len_;
    char pad3_[64];
    boost::atomic<size_t> byte_size_;
    char pad4_[64];
public: // construction
    lockfree_queue(std::size_t size) 
        : size_(size),
           storage_(new slot[size_]),
           read_(0),
           write_(0),
           len_(0),
           byte_size_(0) 
     {
         for (std::size_t i=0; i < size_; i++)
             storage_[i].used = 0;
     }
public:  // api
    bool produce(const T& t)
    {
        T* item = write_prepare();
        if (!item)
            return false;
        *item = t;
        write_publish();
        return true;
    }

    bool consume(T& result)
    {
        T* item = read_fetch();
        if (!item)
            return false;
        result = *item;
        read_consume();
        return true;
    }

    std::size_t len() const
    { 
        return len_.load(boost::memory_order_consume);
    }
    
    std::size_t byte_size() const
    {
        return byte_size_.load(boost::memory_order_consume);
    }
protected: // fetch / publish primitives
    T* read_fetch()
    {
        index_type rd = read_;
        slot* p = &(storage_[rd % size_]);
        if (! p->used.load(boost::memory_order_acquire))
            return 0;
        return &(p->item);
    }
    
    void read_consume()
    {
        index_type rd = read_;
        slot *p = &(storage_[rd % size_]);
        p->used.store(0, boost::memory_order_release);
        len_.fetch_sub(1, boost::memory_order_release);
        byte_size_.fetch_sub(item_size(p->item), boost::memory_order_release);
        read_++;
    }

    T* write_prepare()
    {
        index_type wr = write_;
        slot *p = &(storage_[wr % size_]);
        if (p->used.load(boost::memory_order_acquire))
            return 0;
        return &(p->item);
    }

    void write_publish()
    {
        index_type wr = write_;
        slot *p = &(storage_[wr % size_]);
        p->used.store(1, boost::memory_order_release);
        len_.fetch_add(1, boost::memory_order_release);
        byte_size_.fetch_add(item_size(p->item), boost::memory_order_release);
        write_++;
    }
private: // noncopyable
    lockfree_queue( const lockfree_queue& );
    const lockfree_queue& operator=( const lockfree_queue& );
};

#endif // include guard
