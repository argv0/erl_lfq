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
#include "erl_lfq.h"
#include "lockfree_queue.hpp"
#include <string.h>
#include <stdio.h>
#include <stdint.h>

static ErlNifResourceType* queue_RESOURCE;

struct queue_handle
{
    lockfree_queue<ErlNifBinary> *q;
    uint64_t byte_size;
};

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_EMPTY;
static ERL_NIF_TERM ATOM_VALUE;

static ErlNifFunc nif_funcs[] =
{
    {"new", 0, queue_new},
    {"in", 2, queue_in},
    {"out", 1, queue_out},
    {"byte_size", 1, queue_byte_size},
    {"len", 1, queue_len}
};

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }


ERL_NIF_TERM queue_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queue_handle *handle = 
        (queue_handle *)enif_alloc_resource_compat(env, queue_RESOURCE,
                                                   sizeof(queue_handle));
    memset(handle, '\0', sizeof(queue_handle));
    handle->q = new lockfree_queue<ErlNifBinary>();
    ERL_NIF_TERM result = enif_make_resource(env, handle);
    enif_release_resource_compat(env, handle);
    return enif_make_tuple2(env, ATOM_OK, result);
}

ERL_NIF_TERM queue_in(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queue_handle *handle = 0;
    ErlNifBinary item;
    if (enif_get_resource(env,argv[0],queue_RESOURCE,(void**)&handle) &&
        enif_inspect_binary(env, argv[1], &item))
    {
        ErlNifBinary newbin;
        ERL_NIF_TERM qref = enif_make_resource(env, handle);
        enif_alloc_binary_compat(env, item.size, &newbin);
        memcpy(newbin.data, item.data, item.size);
        handle->q->produce(newbin);
        __sync_add_and_fetch(&(handle->byte_size), item.size);
        return qref;
    }
    else 
    {
        return enif_make_badarg(env);
    }

}

ERL_NIF_TERM queue_out(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queue_handle *handle = 0;
    ErlNifBinary item;
    if (enif_get_resource(env,argv[0],queue_RESOURCE,(void**)&handle));
    {
        ERL_NIF_TERM qref = enif_make_resource(env, handle);
        if (handle->q->consume(item))
        {
            __sync_sub_and_fetch(&(handle->byte_size), item.size);
            return enif_make_tuple2(env, enif_make_tuple2(env, ATOM_VALUE, enif_make_binary(env, &item)), qref);
        }
        else
        {
            return enif_make_tuple2(env, ATOM_EMPTY, qref);
        }
    }
    return enif_make_badarg(env);
}

ERL_NIF_TERM queue_byte_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queue_handle *handle = 0;
    if (enif_get_resource(env,argv[0],queue_RESOURCE,(void**)&handle));
    {
        return enif_make_uint64(env, __sync_fetch_and_add(&(handle->byte_size), 0));
    }
    return enif_make_badarg(env);
}

ERL_NIF_TERM queue_len(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    queue_handle *handle = 0;
    if (enif_get_resource(env,argv[0],queue_RESOURCE,(void**)&handle));
    {
        return enif_make_uint64(env, handle->q->len());
    }
    return enif_make_badarg(env);
}


static void queue_resource_cleanup(ErlNifEnv* env, void* arg)
{
    queue_handle* handle = (queue_handle*)arg;
    delete handle->q;
}

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceFlags flags = (ErlNifResourceFlags)
        (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    queue_RESOURCE = enif_open_resource_type_compat(env, 
                                                    "queue_resource",
                                                    &queue_resource_cleanup,
                                                    flags, 
                                                    NULL);
    // Initialize common atoms
    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");
    ATOM(ATOM_EMPTY, "empty");
    ATOM(ATOM_VALUE, "value");
    return 0;
}

extern "C" {
    ERL_NIF_INIT(erl_lfq, nif_funcs, &on_load, NULL, NULL, NULL);
}
