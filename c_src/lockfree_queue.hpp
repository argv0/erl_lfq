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
        do {
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

template <typename T, template <class> class PointerType=cas_pointer>
class lockfree_queue
{
private:
    struct node 
    {
        node(T val) : value(val), next(0) {};
        T value;
        node *next;
    };
    node *first;
    PointerType<node> divider, last;
public:
    lockfree_queue() 
    {
        first = new node(T());
        divider = PointerType<node>(first);
        last = PointerType<node>(first);
    }

    ~lockfree_queue() 
    {
        while (first != 0) 
        {
            node* tmp = first;
            first = tmp->next;
            delete tmp;
        }
    }
public:
    void produce(const T& t) 
    {
        last.load()->next = new node(t);
        last.store(last.load()->next);
        while (first != divider.load())
        {
            node *tmp = first;
            first = first->next;
            delete tmp;
        }
    }

    bool consume(T& result) 
    {
        if (divider.load() != last.load())
        {
            result = divider.load()->next->value;
            divider.store(divider.load()->next);
            return true;
        }
        return false;
    }

};
