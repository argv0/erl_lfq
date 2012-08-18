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

#include "lockfree_queue.hpp"
#include <cstdlib>
#include <pthread.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

lockfree_queue<unsigned long> *q = 0;
unsigned long producer_total = 0;
unsigned long consumer_total = 0;
unsigned long NITEMS = 20000000;

pthread_t spawn(void* (*entry)(void*)) {
    pthread_t thread;
    pthread_create(&thread, NULL, entry, NULL);
    return thread;
}

void join(pthread_t thread) {
  pthread_join(thread, NULL);
}

int uniform(int range) {
  return (rand() % range) + 1;
}

void* producer(void *p)
{
    for (unsigned long i=0; i < NITEMS; ++i)
    {
        unsigned int j = 1;
        producer_total += j;
        q->produce(j);
    }
    return 0;
}

void* consumer(void *p)
{
    unsigned long j = 0;
    for (unsigned long i=0; i < NITEMS; ++i)
    {
        while (!q->consume(j)) {}
        consumer_total += j;  
    }
    return 0;
}

int test_lfq()
{
    q = new lockfree_queue<unsigned long>(20000000);
    pthread_t t1 = spawn(producer);
    pthread_t t2 = spawn(consumer);
    join(t1);
    join(t2);
    assert(producer_total == consumer_total);
    printf("consumed %lu messages\n", NITEMS);
    return EXIT_SUCCESS;
}


int main(int argc, char* argv[])
{
    if (argc >= 2)
        NITEMS = atol(argv[1]);
    return test_lfq();
}
