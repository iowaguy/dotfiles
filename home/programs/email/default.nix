{ pkgs, ... }:

let passCmd = entry: "${pkgs.pass}/bin/pass ${entry} 2> /dev/null";
in {
  home.packages = with pkgs; [
    afew # initial tagging for notmuch
    notmuch # an email search engine
  ];

  xdg.configFile."afew/config".source = ./afew-config.ini;
  home.file.".notmuch-config".source = ./notmuch-config.ini;

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "${pkgs.isync}/bin/mbsync --all";
      postNew = ''
        ${pkgs.afew}/bin/afew --tag --new
        notmuch tag +newyorker -inbox -new -- "from:newyorker@newsletter.newyorker.com"
        notmuch tag +scholarly-reading -inbox -new -- "from:scholaralerts-noreply@google.com"
        notmuch tag +scholarly-reading -inbox -new -- "from:scholarcitations-noreply@google.com"
        notmuch tag +lightning-dev -inbox -new -- "to:lightning-dev@lists.linuxfoundation.org"
        notmuch tag +calnewport -inbox -new -- "from:author@calnewport.com"
        notmuch tag +economist -inbox -new -- "from:newsletters@e.economist.com"
      '';
    };
  };
  accounts.email = {
    maildirBasePath = "${builtins.getEnv "HOME"}/.Maildir";
    accounts = {
      fastmail = {
        realName = "Ben Weintraub";
        userName = builtins.concatStringsSep "@" [ "ben" "weintraub.xyz" ];
        address = builtins.concatStringsSep "@" [ "ben" "weintraub.xyz" ];
        maildir.path = "fastmail";
        passwordCommand = passCmd "fastmail-app-pass";
        primary = true;
        mbsync = {
          enable = true;
          create = "maildir";
          remove = "both";
          expunge = "both";
        };
        msmtp.enable = true;
        notmuch.enable = true;

        folders = {
          inbox = "INBOX";
          drafts = "Drafts";
          sent = "Sent";
          trash = "Trash";
        };
        smtp = {
          host = "smtp.fastmail.com";
          port = 465;
        };

        imap = {
          host = "imap.fastmail.com";
          port = 993;
          tls.enable = true;
        };
      };
    };
  };

  # Check for new mail every five minutes
  systemd.user = {
    services = {
      notmuch = {
        Unit = {
          Description = "Get new mail";
        };
        Service = {
          ExecStart = ''
            ${pkgs.notmuch}/bin/notmuch --config ${builtins.getEnv "HOME"}/.config/notmuch/notmuchrc new
            '';
          Type = "oneshot";
        };
      };
    };
    timers = {
      notmuch = {
        Unit = {
          Description = "Get new mail";
        };
        Timer = {
          # run one minute after boot
          OnBootSec = "1min";

          # Rerun every five minutes the unit is active
          OnCalendar = "*:0/5";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
      };
    };
  };
}
