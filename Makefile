
VERSION := $(shell nixos-version | grep -o "^[0-9]\+\.[0-9]\+")
# FLAKES := unstable rofi nur impermanence hm
FLAKES := stable unstable rofi nur impermanence hm

FLAKE_UPDATE_CMDS := $(foreach flake,$(FLAKES),update-$(flake))

# Capture any arguments after the target name
ARGS := $(filter-out $@,$(MAKECMDGOALS))


define nosleep
	systemd-inhibit --what='sleep:idle' zsh -i -c $1
endef

define flakeupdate
	sudo nix flake update $1
endef

.PHONY: $(FLAKE_UPDATE_CMDS) upgrade

update-unstable:
	$(call flakeupdate, nixpkgs-unstable)

update-stable:
	$(if $(ARGS),,$(error No new version provided. Usage: make update-stable <new NixOS version>))
	@read -p "Are you sure you want to update stable? That will rebuild the kernel which takes awhile. [y/N] " ans; \
	if [ "$$ans" != "y" ] && [ "$$ans" != "Y" ]; then \
		echo "Aborted."; \
		exit 1; \
	fi; \
	$(call flakeupdate, nixpkgs-$(word 2,$(ARGS)))

update-rofi:
	$(call flakeupdate, rofi-theme)

update-hm:
	$(call flakeupdate, home-manager)

update-nur:
	$(call flakeupdate, nur)

update-impermanence:
	$(call flakeupdate, impermanence)

update-all: $(FLAKE_UPDATE_CMDS)
	$(MAKE) fsw

# fsw:
# 	if [[ ! -L "$(HOME)/.config/mimeapps.list" ]]; then\
# 		@echo "Removing non-symlinked mimeapps.list"\
# 		rm -f "$HOME/.config/mimeapps.list"\
# 		echo "Exit status of rm is $?"\
# 	fi\
# 	@echo -n "mimeapps.list should be a symlink or empty:"
# 	if [[ ! -L "$HOME/.xmonad/xmonad-x86_64-linux" ]]; then\
# 		echo "Removing non-symlinked xmonad binary"\
# 		rm -f "$HOME/.xmonad/xmonad-x86_64-linux"\
# 		echo "Exit status of rm is $?"\
# 	fi\
# 	@echo -n "xmonad-x86_64-linux should be a symlink or empty:"\
# 	@echo "$(ls -l1 $HOME/.xmonad/xmonad-x86_64-linux)"\
# 	sudo nixos-rebuild --flake ".#" switch $(ARGS)

test:
	@echo $(VERSION)

upgrade:
	$(if $(ARGS),,$(error No new version provided. Usage: make upgrade <new NixOS version>))
	@read -p "Are you sure you want to upgrade from NixOS $(VERSION) to $(word 2,$(ARGS))? [y/N] " ans; \
	if [ "$$ans" != "y" ] && [ "$$ans" != "Y" ]; then \
		echo "Aborted."; \
		exit 1; \
	fi; \
	sed -i "s/$(VERSION)/$(word 2,$(ARGS))/g" flake.nix
	$(MAKE) update-all
