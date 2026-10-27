# Keep version metadata current without watching the entire checkout in Cargo.
export HEW_BUILD_SOURCE_ROOT := $(CURDIR)
export HEW_BUILD_GIT_DIRTY := $(if $(shell git status --porcelain --untracked-files=no 2>/dev/null),dirty,clean)
