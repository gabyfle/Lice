///////////////////////////////////////////////////////////////////////////////
//                                                                           //
//  This file is part of Lice.                                               //
//                                                                           //
//  Copyright (C) 2023                                                       //
//    Gabriel Santamaria                                                     //
//                                                                           //
//                                                                           //
//  Licensed under the Apache License, Version 2.0 (the "License");          //
//  you may not use this file except in compliance with the License.         //
//  You may obtain a copy of the License at                                  //
//                                                                           //
//    http://www.apache.org/licenses/LICENSE-2.0                             //
//                                                                           //
//  Unless required by applicable law or agreed to in writing, software      //
//  distributed under the License is distributed on an "AS IS" BASIS,        //
//  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. //
//  See the License for the specific language governing permissions and      //
//  limitations under the License.                                           //
//                                                                           //
///////////////////////////////////////////////////////////////////////////////

#ifndef _LVM_VM_H
#define _LVM_VM_H

#define REGISTERS_NUMBER 4 // Number of registers in the VM

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include "chunk.h"

typedef struct VM {
  value registers[REGISTERS_NUMBER];

  chunk_t *chunk;
} VM;

#endif
