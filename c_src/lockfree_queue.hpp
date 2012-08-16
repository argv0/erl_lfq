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
#include <cstddef>

template <typename T>
class cas_pointer
{
public:
    cas_pointer() {}
    explicit cas_pointer(T* p) : ptr_(p) {}
public:
    inline T* load() const 
    { 
        return __sync_fetch_and_add(&ptr_, 0);
    }
    inline void store(T* p) 
    {
        do 
        {
            __sync_val_compare_and_swap(&ptr_, ptr_, p);
        } while(ptr_ != p);
    }
private:
    mutable T* ptr_;
};

template <typename T>
class barrier_pointer
{
public:
    barrier_pointer() {}
    explicit barrier_pointer(T* p) : ptr_(p) {}
public:
    inline T* load() const 
    { 
        T* result = ptr_;
        memory_barrier();
        return result;
    }
    inline void store(T* p) 
    {
        memory_barrier();
        ptr_ = p;
    }
private:
    void memory_barrier() const { __sync_synchronize(); }
private:
    T* ptr_;
};

template <typename T, template <class> class PointerType=barrier_pointer>
class lockfree_queue
{
private:
    struct node 
    {
        node(T val) 
            : value(val), 
              next(0) {};
        T value;
        node *next;
    };
    typedef PointerType<node> atomic_node_pointer;

public:
    lockfree_queue() 
        : first_(new node(T())),
          divider_(atomic_node_pointer(first_)),
          last_(atomic_node_pointer(first_)) {}

    ~lockfree_queue() 
    {
        while (first_ != 0) 
        {
            node* tmp = first_;
            first_ = tmp->next;
            delete tmp;
        }
    }
public:
    void produce(const T& t) 
    {
        last_.load()->next = new node(t);
        last_.store(last_.load()->next);
        __sync_add_and_fetch(&len_, 1);
        while (first_ != divider_.load())
        {
            node *tmp = first_;
            first_ = first_->next;
            delete tmp;
        }
    }

    bool consume(T& result) 
    {
        if (divider_.load() != last_.load())
        {
            result = divider_.load()->next->value;
            divider_.store(divider_.load()->next);
            __sync_sub_and_fetch(&len_, 1);
            return true;
        }
        return false;
    }
    
    std::size_t len() const
    {
        return __sync_fetch_and_add(&len_, 0);
    }
    
private:
    node *first_;
    atomic_node_pointer divider_, last_;
    std::size_t len_;
};
