###############################################################################
##                                                                           ##
##  This file is part of Lice.                                               ##
##                                                                           ##
##  Copyright (C) 2023                                                       ##
##    Gabriel Santamaria                                                     ##
##                                                                           ##
##                                                                           ##
##  Licensed under the Apache License, Version 2.0 (the "License");          ##
##  you may not use this file except in compliance with the License.         ##
##  You may obtain a copy of the License at                                  ##
##                                                                           ##
##    http://www.apache.org/licenses/LICENSE-2.0                             ##
##                                                                           ##
##  Unless required by applicable law or agreed to in writing, software      ##
##  distributed under the License is distributed on an "AS IS" BASIS,        ##
##  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ##
##  See the License for the specific language governing permissions and      ##
##  limitations under the License.                                           ##
##                                                                           ##
###############################################################################

## This file is used to find the OCaml compiler and its include directory.

find_program(OCAMLC ocamlc)
if(NOT OCAMLC)
  message(FATAL_ERROR "Looks like ocamlc is not installed on your system.")
endif()

execute_process(
  COMMAND ${OCAMLC} -where
  OUTPUT_VARIABLE OCAML_INCLUDE_DIRS
  OUTPUT_STRIP_TRAILING_WHITESPACE
)

if(NOT OCAML_INCLUDE_DIRS)
  message(FATAL_ERROR "We couldn't find the OCaml include directory.")
endif()

mark_as_advanced(OCAMLC OCAML_INCLUDE_DIRS)
