cmake_minimum_required(VERSION 3.12)
project(CASAL2)

# Variables
SET(EXE_NAME "casal2")
SET(OUTPUT_PATH "" CACHE STRING "Combination of OS and Compiler from CommandLine e.g. -D windows_gcc")

# Change the output folders for our build to something cleaner and platform dependent
SET(CMAKE_ROOT   bin/cmake)

SET(PROJECT_HOME_DIRECTORY ${CMAKE_HOME_DIRECTORY})
STRING(REPLACE "/CASAL2/CASAL2" "/CASAL2/BuildSystem/" BUILD_SYSTEM_DIRECTORY ${CMAKE_HOME_DIRECTORY})
MESSAGE("-- Source Root Folder: ${CMAKE_HOME_DIRECTORY}")
MESSAGE("-- BuildSystem Folder: ${BUILD_SYSTEM_DIRECTORY}")

# GLOBAL BUILD VARIABLES AND LIBRARIES
SET(COMPILE_OPTIONS "-Wall -Werror -fmessage-length=0 -Wno-unused-local-typedefs")
IF (DEBUG EQUAL 1)
	MESSAGE("-- Build Type: Debug")
 	SET(COMPILE_OPTIONS "-O0 -g -static ${COMPILE_OPTIONS}")
ELSEIF (RELEASE EQUAL 1)
	MESSAGE("-- Build Type: Release")
 	SET(COMPILE_OPTIONS "-O3 -g0 ${COMPILE_OPTIONS}")
ELSEIF (TESTMODE EQUAL 1)
	MESSAGE("-- Build Type: Unit Test")
 	SET(COMPILE_OPTIONS "-DTESTMODE -O3 -g0 ${COMPILE_OPTIONS}")
ELSE()
 	SET(COMPILE_OPTIONS "-O3 -g0 ${COMPILE_OPTIONS}")
ENDIF ()

# Set variables based on OS
IF (WIN32)
	MESSAGE("--> Building on Windows")
	IF (MSVC)
		SET(COMPILE_OPTIONS "/std:c++17 /MP /O2 /GT /GL")
	ELSE(MSVC)
		SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -D__MINGW32__ -D__GNUDOS__ -std=c++17")
 		SET(thirdPartyLibraries ${thirdPartyLibraries} pthread ws2_32 gdi32 comctl32 user32 winmm ws2_32 imm32)
	ENDIF ()
ELSE (WIN32)
	MESSAGE("--> Building on Linux")

 	SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -fPIC -std=c++17")
 	SET(thirdPartyLibraries ${thirdPartyLibraries} pthread rt)
ENDIF ()

IF (ASAN)
	SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -static-libasan -fsanitize=address -fno-omit-frame-pointer")
	SET(thirdPartyLibraries asan ${thirdPartyLibraries} )
ELSEIF(TSAN)
	SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -static-libtsan -fsanitize=thread -fno-omit-frame-pointer")
	SET(thirdPartyLibraries tsan ${thirdPartyLibraries})
ELSEIF(MSAN)
	SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -static-libmsan -fsanitize=memory -fno-omit-frame-pointer")
	SET(thirdPartyLibraries msan ${thirdPartyLibraries})
ENDIF()

MESSAGE("-- Compiler Options: ${COMPILE_OPTIONS}")

INCLUDE_DIRECTORIES(SYSTEM "${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/include")
INCLUDE_DIRECTORIES("${CMAKE_HOME_DIRECTORY}/source")

# Now we want to see if we're building an auto-diff library or something of that nature.
# BUILD PARAMETERS
IF (ADOLC)
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -DUSE_AUTODIFF -DUSE_ADOLC")
 SET(specialLibraries "-l:libadolc.a")
ELSEIF(BETADIFF)
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -DUSE_AUTODIFF -DUSE_BETADIFF")
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -Wno-unused-local-typedefs -Wno-write-strings -Wno-unused-variable -Wno-unused-but-set-variable -fpermissive -Wno-error")
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -iquote${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/include/ADOLC18/SRC")
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -iquote${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/include/ADOLC18/SRC/DRIVERS")
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -iquote${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/include/ADOLC18/SRC/SPARSE")
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -iquote${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/include/ADOLC18/SRC/TAPEDOC")
 SET(specialLibraries "-l:libbetadiff.a")
ELSEIF(CPPAD)
 SET(COMPILE_OPTIONS "${COMPILE_OPTIONS} -DUSE_AUTODIFF -DUSE_CPPAD")
 IF (WIN32)
  SET(specialLibraries
   "ipopt"
   "coinmumps"
   "coinmetis"
   "m"
   "gfortran"
   "quadmath"
   "msvcrt"
   "coinlapack"
   "coinblas"
  )
 ELSE (WIN32)
  SET(specialLibraries
   "quadmath"
   "dl"
   "ipopt"
   "coinmumps"
   "coinlapack"
   "coinblas"
   "coinmetis"
   "m"
   "gfortran"
   "dl"
  )
 ENDIF ()
ENDIF ()
MESSAGE("SPECIAL_LIBRARIES: ${specialLibraries}")

# This snippet of code will find all of our source and test files
# and auto-populate variables with them for the build
FILE(GLOB_RECURSE sourceFiles ${PROJECT_HOME_DIRECTORY}/source/*.cpp)
IF (NOT TESTMODE EQUAL 1)
	FILE(GLOB_RECURSE testFiles ${PROJECT_HOME_DIRECTORY}/source/*.Test.cpp)
	list(REMOVE_ITEM sourceFiles ${testFiles})
ENDIF ()

# Get a list of our third party libraries
IF (RELEASE)
	SET(thirdPartyFolderAbs "${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/lib/release/")
	SET(thirdPartyFolder "../BuildSystem/bin/${OUTPUT_PATH}/thirdparty/lib/release/")
ELSE (RELEASE)
	SET(thirdPartyFolderAbs "${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/lib/debug/")
	SET(thirdPartyFolder "../BuildSystem/bin/${OUTPUT_PATH}/thirdparty/lib/debug/")
ENDIF ()
MESSAGE("Third Party Folder: ${thirdPartyFolder}")
LINK_DIRECTORIES("${thirdPartyFolderAbs}")
LINK_DIRECTORIES("${BUILD_SYSTEM_DIRECTORY}/bin/${OUTPUT_PATH}/thirdparty/lib/special/")

IF (MSVC)
	FILE(GLOB thirdPartyLibrariesRaw RELATIVE ${thirdPartyFolderAbs} ${thirdPartyFolder}*.lib)
	FOREACH(library ${thirdPartyLibrariesRaw})
  		STRING(REPLACE ${thirdPartyFolderAbs} "" shortLibrary ${library})
  		SET(thirdPartyLibraries " ") #${thirdPartyLibraries} ${shortLibrary})
	ENDFOREACH()
	SET(thirdPartyLibraries " ")
ELSE(MSVC)
	MESSAGE("-- Loading third party libraries")
	FILE(GLOB thirdPartyLibrariesRaw RELATIVE ${thirdPartyFolderAbs} ${thirdPartyFolder}*.a)
	FOREACH(library ${thirdPartyLibrariesRaw})
	  MESSAGE("-- ${library}")
	  STRING(REPLACE ${thirdPartyFolderAbs} "" shortLibrary ${library})
	  SET(thirdPartyLibraries ${thirdPartyLibraries} -l:${shortLibrary})
	ENDFOREACH()
ENDIF()

FOREACH(library ${specialLibraries})
	SET(thirdPartyLibraries ${thirdPartyLibraries} ${library})
ENDFOREACH()

IF (TESTMODE EQUAL 1)
  LIST(SORT thirdPartyLibraries)
ENDIF()

SET(LINK_OPTIONS " ")
IF (LIBRARY)
  ADD_LIBRARY(${EXE_NAME} SHARED ${sourceFiles} )
  IF(WIN32)
    SET(LINK_OPTIONS "-static")
  ENDIF(WIN32)
ELSE(LIBRARY)
	IF(NOT TESTMODE)
		IF (NOT MSVC)
			IF(WIN32)
			MESSAGE("Building ICON")
			ADD_CUSTOM_TARGET(icon ALL COMMAND windres --output "${BUILD_SYSTEM_DIRECTORY}/icon.o" --input "${BUILD_SYSTEM_DIRECTORY}/casal2.rc")
			MESSAGE("Icon Built")
			#SET(LINK_OPTIONS "-static-libgcc -static-libstdc++ -static ${CMAKE_HOME_DIRECTORY}/../BuildSystem/icon.o")
			SET(LINK_OPTIONS "-static ${BUILD_SYSTEM_DIRECTORY}/icon.o")
			ENDIF(WIN32)
		ELSE()
			SET(LINK_OPTIONS " ")
		ENDIF()
	ENDIF()

	MESSAGE("Link Options: ${LINK_OPTIONS}")
	MESSAGE("Third party libraries: ${thirdPartyLibraries}")

	ADD_EXECUTABLE(${EXE_NAME} ${sourceFiles} )
ENDIF(LIBRARY)


SET_TARGET_PROPERTIES(${EXE_NAME} PROPERTIES COMPILE_FLAGS ${COMPILE_OPTIONS} LINK_FLAGS ${LINK_OPTIONS})
TARGET_LINK_LIBRARIES(${EXE_NAME} ${thirdPartyLibraries})
IF (NOT LIBRARY)
	IF(NOT TESTMODE)
		IF (NOT MSVC)
			IF(WIN32)
			ADD_DEPENDENCIES(${EXE_NAME} icon)
			ENDIF(WIN32)
		ENDIF()
	ENDIF()
ENDIF()