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
#ifndef ERL_LOCKFREEQUEUE_NIFS_H_
#define ERL_LOCKFREEQUEUE_NIFS_H_

extern "C" {

#include "erl_nif_compat.h"

ERL_NIF_TERM queue_new(ErlNifEnv*, int, const ERL_NIF_TERM[]);
ERL_NIF_TERM queue_in(ErlNifEnv*, int, const ERL_NIF_TERM[]);
ERL_NIF_TERM queue_out(ErlNifEnv*, int, const ERL_NIF_TERM[]);
ERL_NIF_TERM queue_byte_size(ErlNifEnv*, int, const ERL_NIF_TERM[]);
ERL_NIF_TERM queue_len(ErlNifEnv*, int, const ERL_NIF_TERM[]);

} // extern "C"

#endif // include guard
