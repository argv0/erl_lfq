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

#include "atomic_pointer.h"

template <typename T>
class atomic_pointer
{
public:
    atomic_pointer() {}
    explicit atomic_pointer(T p) 
    : ptr_(p) {}
public:
    inline T load() const { 
        return static_cast<T>(ptr_.Acquire_Load());
    }
    inline void store(T p) {
        ptr_.Release_Store(p);
    }
private:
    leveldb::port::AtomicPointer ptr_;
};

template <typename T>
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
    atomic_pointer<node *> divider, last;

public:
    lockfree_queue() {
        first = new node(T());
        divider = atomic_pointer<node*>(first);
        last = atomic_pointer<node *>(first);
    }

    ~lockfree_queue() {
        while (first != 0) {
            node* tmp = first;
            first = tmp->next;
            delete tmp;
        }
    }
public:
    void produce(const T& t) {
        last.load()->next = new node(t);
        last.store(last.load()->next);
        while (first != divider.load())
        {
            node *tmp = first;
            first = first->next;
            delete tmp;
        }
    }

    bool consume(T& result) {
        if (divider.load() != last.load())
        {
            result = divider.load()->next->value;
            divider.store(divider.load()->next);
            return true;
        }
        return false;
    }
};
