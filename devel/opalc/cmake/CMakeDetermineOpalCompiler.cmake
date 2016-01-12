# Copyright (c) 2011, UEBB Group
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#   * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#   * Redistributions in binary form must reproduce the above
#     copyright notice, this list of conditions and the following
#     disclaimer in the documentation and/or other materials provided
#     with the distribution.
#   * Neither the name of the TU Berlin nor the names of its
#     contributors may be used to endorse or promote products derived
#     from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# Determine the compiler to use for Opal programs
#
# Note: a generator may set CMAKE_OPAL_COMPILER before
# loading this file to force a compiler.
# 
# Use environment variable OPALC first if defined by user, next use
# the CMake variable CMAKE_GENERATOR_OPALC which can be defined by a
# generator as a default compiler.
#
# Sets the following variables:
# CMAKE_OPAL_COMPILER
# CMAKE_OPAL_COMPILER_ARG1

IF(NOT CMAKE_OPAL_COMPILER)

  # Prefer the environment variable OPALC.
  IF($ENV{OPALC} MATCHES ".+")
    GET_FILENAME_COMPONENT(CMAKE_OPAL_COMPILER_INIT $ENV{OPALC} PROGRAM PROGRAM_ARGS CMAKE_OPAL_FLAGS_ENV_INIT)
    IF(CMAKE_OPAL_FLAGS_ENV_INIT)
      SET(CMAKE_OPAL_COMPILER_ARG1 "${CMAKE_OPAL_FLAGS_ENV_INIT}" CACHE STRING "First argument to Opal compiler")
    ENDIF(CMAKE_OPAL_FLAGS_ENV_INIT)
    IF(NOT EXISTS ${CMAKE_OPAL_COMPILER_INIT})
      MESSAGE(FATAL_ERROR "Could not find compiler set in environment variable OPALC:\n$ENV{OPALC}")
    ENDIF(NOT EXISTS ${CMAKE_OPAL_COMPILER_INIT})
  ENDIF($ENV{OPALC} MATCHES ".+")

  # Next try to prefer the compiler specified by the generator.
  IF(CMAKE_GENERATOR_OPALC)
    IF(NOT CMAKE_OPAL_COMPILER_INIT)
      SET(CMAKE_OPAL_COMPILER_INIT ${CMAKE_GENERATOR_OPALC})
    ENDIF(NOT CMAKE_OPAL_COMPILER_INIT)
  ENDIF(CMAKE_GENERATOR_OPALC)

  # Finally list compilers to try.
  IF(CMAKE_OPAL_COMPILER_INIT)
    SET(CMAKE_OPAL_COMPILER_LIST ${CMAKE_OPAL_COMPILER_INIT})
  ELSE(CMAKE_OPAL_COMPILER_INIT)
    SET(CMAKE_OPAL_COMPILER_LIST opalc)
  ENDIF(CMAKE_OPAL_COMPILER_INIT)

  # Find the compiler.
  FIND_PROGRAM(CMAKE_OPAL_COMPILER NAMES ${CMAKE_OPAL_COMPILER_LIST} DOC "Opal compiler")

  IF(CMAKE_OPAL_COMPILER_INIT AND NOT CMAKE_OPAL_COMPILER)
    SET(CMAKE_OPAL_COMPILER "${CMAKE_OPAL_COMPILER_INIT}" CACHE FILEPATH "Opal compiler" FORCE)
  ENDIF(CMAKE_OPAL_COMPILER_INIT AND NOT CMAKE_OPAL_COMPILER)

ELSE(NOT CMAKE_OPAL_COMPILER)

  # We only get here, if CMAKE_OPAL_COMPILER was specified using -D, a
  # pre-made CMakeCache.txt (e. g. via CTest), or set in
  # CMAKE_TOOLCHAIN_FILE.
  # If CMAKE_OPAL_COMPILER is a list of length 2, use the first item
  # as CMAKE_OPAL_COMPILER and the second one as
  # CMAKE_OPAL_COMPILER_ARG1.

  LIST(LENGTH CMAKE_OPAL_COMPILER _CMAKE_OPAL_COMPILER_LIST_LENGTH)
  IF("${_CMAKE_OPAL_COMPILER_LIST_LENGTH}" EQUAL 2)
    LIST(GET CMAKE_OPAL_COMPILER 1 CMAKE_OPAL_COMPILER_ARG1)
    LIST(GET CMAKE_OPAL_COMPILER 0 CMAKE_OPAL_COMPILER)
  ENDIF("${_CMAKE_OPAL_COMPILER_LIST_LENGTH}" EQUAL 2)

  # If a compiler was specified by the user but without path, now try
  # to find it with the full path if it is found, force it into the
  # cache, if not, don't overwrite the setting (which was given by the
  # user) with "NOTFOUND" if the compiler already had a path.
  GET_FILENAME_COMPONENT(_CMAKE_USER_OPAL_COMPILER_PATH "${CMAKE_OPAL_COMPILER}" PATH)
  IF(NOT _CMAKE_USER_OPAL_COMPILER_PATH)
    FIND_PROGRAM(CMAKE_OPAL_COMPILER_WITH_PATH NAMES ${CMAKE_OPAL_COMPILER})
    MARK_AS_ADVANCED(CMAKE_OPAL_COMPILER_WITH_PATH)
    IF(CMAKE_OPAL_COMPILER_WITH_PATH)
      SET(CMAKE_OPAL_COMPILER ${CMAKE_OPAL_COMPILER_WITH_PATH} CACHE STRING "Opal compiler" FORCE)
    ENDIF(CMAKE_OPAL_COMPILER_WITH_PATH)
  ENDIF(NOT _CMAKE_USER_OPAL_COMPILER_PATH)

ENDIF(NOT CMAKE_OPAL_COMPILER)
MARK_AS_ADVANCED(CMAKE_C_COMPILER)

IF(NOT _CMAKE_TOOLCHAIN_LOCATION)
  GET_FILENAME_COMPONENT(_CMAKE_TOOLCHAIN_LOCATION "${CMAKE_OPAL_COMPILER}" PATH)
ENDIF (NOT _CMAKE_TOOLCHAIN_LOCATION)


# Configure variables set in this file.
CONFIGURE_FILE(
  CMakeOpalCompiler.cmake.in
  "${CMAKE_PLATFORM_ROOT_BIN}/CMakeOpalCompiler.cmake"
  @ONLY
  )
SET(CMAKE_OPAL_COMPILE_ENV_VAR "OPALC")
