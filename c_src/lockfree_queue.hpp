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

template <typename T, size_t size=1000000>
class lockfree_queue
{
    typedef T item_type;
    typedef int index_type;
    
    struct slot
    {
         item_type item;
         boost::atomic_bool used;
    };
    slot array_[size];
    int  read_;
    int  write_;
    boost::atomic_ulong len_;
 public:
     lockfree_queue() 
         : read_(0),
           write_(0)
     {
         for (std::size_t i=0; i < size; i++)
             array_[i].used = false;
     }

    T* read_fetch()
    {
        index_type rd = read_;
        slot* p = &(array_[rd % size]);
        if (! p->used.load(boost::memory_order_acquire))
            return 0;
        return &(p->item);
    }
    
    void read_consume()
    {
        index_type rd = read_;
        slot *p = &(array_[rd % size]);
        p->used.store(0, boost::memory_order_release);
        len_.fetch_sub(1, boost::memory_order_release);
        read_++;
    }

    T* write_prepare()
    {
        index_type wr = write_;
        slot *p = &(array_[wr % size]);
        if (p->used.load(boost::memory_order_acquire))
            return 0;
        return &(p->item);
    }

    void write_publish()
    {
        index_type wr = write_;
        slot *p = &(array_[wr % size]);
        p->used.store(true, boost::memory_order_release);
        len_.fetch_add(1, boost::memory_order_release);
        write_++;
    }
    
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

    std::size_t len() 
    { 
        return len_.load(boost::memory_order_consume);
    }
    
 };

#endif // include guard
