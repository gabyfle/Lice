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

cmake_minimum_required(VERSION 3.16.3)
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")

project(lvm)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_BUILD_SHARED ON)

set(SOURCE_FILES vm.c)

find_package(OCaml REQUIRED)

add_library(
    lvm
    STATIC
    ${SOURCE_FILES}
)
target_include_directories(lvm PRIVATE ${OCAML_INCLUDE_DIRS})

add_library(
    lvmsh
    SHARED
    ${SOURCE_FILES}
)
target_include_directories(lvmsh PRIVATE ${OCAML_INCLUDE_DIRS})
