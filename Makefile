PROJECT = pal

DEPS = pt
dep_pt = git git://github.com/manifest/pt.git develop

COMPILE_FIRST = pal_workflow pal_authentication
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)'

include erlang.mk

