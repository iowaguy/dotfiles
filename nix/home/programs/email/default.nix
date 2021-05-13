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

      gmail = {
        address = builtins.concatStringsSep "@" [ "benweintraub34" "gmail.com" ];
        imap.host = "imap.gmail.com";
        mbsync = {
          enable = true;
          create = "maildir";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        realName = "Ben Weintraub";
        passwordCommand = passCmd "mbsync";
        smtp = {
          host = "smtp.gmail.com";
        };
        userName = builtins.concatStringsSep "@" [ "benweintraub34" "gmail.com" ];
        flavor = "gmail.com";
        folders = {
          inbox = "Inbox";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/SentMail";
          trash = "[Gmail]/Trash";
        };
        # lieer.enable = true; # two-way sync labels, just for gmail
        imap.port = 993;
        maildir.path = "gmail";
      };
      # northeastern = {
      #   address = builtins.concatStringsSep "@" [ "weintraub.b" "northeastern.edu" ];
      #   mbsync = {
      #     enable = true;
      #     create = "maildir";
      #     extraConfig.account = {
      #       AuthMechs = "Login";
      #     };
      #   };
      #   msmtp.enable = true;
      #   notmuch.enable = true;
      #   realName = "Ben Weintraub";
      #   passwordCommand = passCmd "Login/northeastern.edu/weintraub.b";
      #   smtp = {
      #     host = "smtp.office365.com";
      #     # security = "starttls";
      #   };
      #   userName = builtins.concatStringsSep "@" [ "weintraub.b" "northeastern.edu" ];
      #   folders = {
      #     inbox = "Inbox";
      #     drafts = "Drafts";
      #     sent = "Sent";
      #     trash = "Deleted";
      #   };
      #   imap = {
      #     host = "outlook.office365.com";
      #     port = 993;
      #     # tls.enable = true;
      #   };
      #   maildir.path = "northeastern";
      # };
    };
  };
}
