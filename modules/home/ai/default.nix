{ config, lib, pkgs, ... }:

{
  home.file = {
    ".claude/skills/ask/SKILL.md" = {
      source = ./ask.md;
      executable = false;
    };
  };
}
