# Makefile for subsystems of 'oc2' 
#
# $Id$

#---------------------------------------------------------------------
#		locations and general macros
# --------------------------------------------------------------------


# The base directory of the project's source repository area.
#   NOTE this is not (necessary) the same as value of the OCS
#   environment variable

BASE = /home/korso/opal/ocs

# Inclusion of project wide definitions

include $(BASE)/etc/shape/Makedefs

# Path to this node system relative to the root of the source
# repository defined above (e.g. src/oc/oc2/optim).

NODEPATH = src/oc/oc2

# A short name for this node system

NODENAME = oc2_dir

# --------------------------------------------------------------------
#		the system's components
# --------------------------------------------------------------------

# give one or more targets

TARGET = 

# The release number generator. The version number of this file will
# be used as release identifier for the whole system. 

VERSIONOBJECT =				# derived (if source contains 
					# program code)

# The names of the subdirectories containing subsystems which are also
# to be built. NOTE that this are rms subsystems (direct subdirectorys
# in a tree-like hierarchy) and not the OCS subsystems which are
# integrated automatically

SUBSYSTEMS = optim trans gen driver

# Aliases for (filesystem links to) $(TARGET).

ALIASES = 

# The regular source and header files.

SOURCES = 

HEADERS = 

# Auxiliary source and header files that are not genuine part of the
# system (eg. a test environment). These will also be processed on
# system building, but will *not* be included in releases.

AUXSOURCES = 

AUXHEADERS = 

# Sources of variant source components stored under equal names in
# different locations. During system builds, only one of each (equally
# named) group is chosen. Source distributions need all of them.

VARIANTSOURCES =

VARIANTHEADERS =

# The manuals

MANUALS = $(MAN1) $(MAN3) $(MAN4) $(MAN5) $(MAN6) $(MAN7) $(MAN8)
MAN1 = 
MAN3 = 
MAN4 = 
MAN5 = 
MAN6 = 
MAN7 = 
MAN8 = 

# All source components of the system 

COMPONENTS = $(SOURCES) $(HEADERS) $(MANUALS) \
	     Shapefile Makefile 	

# The derived files. All files, that are autamatically produced during
# a build process should be listed here.

OBJECTS = 



# --------------------------------------------------------------------
#			the targets
# --------------------------------------------------------------------


# The default action (do not change)

all: +all $(ALLTARGETS)


# The final system building action.

targets: $(TARGET)

# How to make the diverse targets, if no shape builtin rule available ...
# (see shape(1))


# Inclusion of shared global targets 

include $(BASE)/etc/shape/Makerules.misc
