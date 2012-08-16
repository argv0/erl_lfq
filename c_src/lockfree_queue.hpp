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

#include "cacheline.h"
#include <cstddef>
#include <cstdio>

template <typename T>
class cas_pointer
{
public:
    cas_pointer() {}
    explicit cas_pointer(T p) : ptr_(p) {}
public:
    inline T load() const 
    { 
        return __sync_fetch_and_add(&ptr_, 0);
    }
    inline void store(T p) 
    {
        do 
        {
            __sync_val_compare_and_swap(&ptr_, ptr_, p);
        } while(ptr_ != p);
    }
private:
    mutable T ptr_;
};


template <typename T>
class barrier_pointer
{
public:
    barrier_pointer() {}
    explicit barrier_pointer(T p) : ptr_(p) {}
public:
    inline T load() const 
    { 
        T result = ptr_;
        memory_barrier();
        return result;
    }
    inline void store(T p) 
    {
        memory_barrier();
        ptr_ = p;
    }
private:
    void memory_barrier() const { __sync_synchronize(); }
private:
    T ptr_;
};

template <typename T, size_t size>
class spsc_queue
{
    typedef T item_type;
    typedef int index_type;

     struct slot
     {
         item_type item;
         volatile int used;
     };
     slot array_[size];
     int  read_;
     int  write_;
 public:
     spsc_queue() 
         : read_(0),
           write_(0)
     {
         for (int i=0; i < size; i++)
             array_[i].used = 0;
     }

    T* read_fetch()
    {
        index_type rd = read_;
        slot* p = &(array_[rd % size]);
        if (!(__sync_fetch_and_add(&(p->used), 0)))
            return 0;
        return &(p->item);
    }
    
    void read_consume()
    {
        index_type rd = read_;
        slot *p = &(array_[rd % size]);
        do 
            __sync_val_compare_and_swap(&(p->used), p->used, 0);
        while(p->used != 0);        
        read_++;
    }

    T* write_prepare()
    {
        index_type wr = write_;
        slot *p = &(array_[wr % size]);
        if (__sync_fetch_and_add(&(p->used), 0))
            return 0;
        return &(p->item);
    }

    void write_publish()
    {
        index_type wr = write_;
        slot *p = &(array_[wr % size]);
        do 
            __sync_val_compare_and_swap(&(p->used), p->used, 1);
        while(p->used != 1);                
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
     typedef PointerType<node*> atomic_node_pointer;
 public:
     lockfree_queue() 
         : first_(new node(T())),
           divider_(atomic_node_pointer(first_)),
           last_(atomic_node_pointer(first_)),
           len_(0),
           byte_size_(0) {}

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
         //__sync_add_and_fetch(&byte_size_, t.size);
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
             //__sync_sub_and_fetch(&byte_size_, result.size);
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
    mutable std::size_t len_;
    mutable std::size_t byte_size_;
};

#endif // include guard
