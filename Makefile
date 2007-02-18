TOP=..
include $(TOP)/mk/boilerplate.mk

# ---------------------------------------------------------------

ALL_DIRS = \
    Text/ParserCombinators/Parsec

PACKAGE		:= hsemail
RELEASEDAY	:= 2005-04-29
VERSION		:= 0.0-$(RELEASEDAY)
PACKAGE_DEPS	:= base mtl parsec

SRC_HC_OPTS	+= -Wall

SRC_HADDOCK_OPTS += -t "Internet Message Parsers ($(PACKAGE) package)"

# ---------------------------------------------------------------

-include $(TOP)/mk/crypto.mk
include $(TOP)/mk/target.mk
