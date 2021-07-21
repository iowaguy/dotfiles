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

      gmail = {
        address = builtins.concatStringsSep "@" [ "benweintraub34" "gmail.com" ];
        mbsync = {
          enable = true;
          create = "maildir";
          remove = "both";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        realName = "Ben Weintraub";
        passwordCommand = passCmd "mbsync";
        userName = builtins.concatStringsSep "@" [ "benweintraub34" "gmail.com" ];
        flavor = "gmail.com"; # This sets the IMAP and SMTP servers automatically
        folders = {
          inbox = "Inbox";
          drafts = "[Gmail]/Drafts";
          sent = "[Gmail]/SentMail";
          trash = "[Gmail]/Trash";
        };

        maildir.path = "gmail";
      };
    };
  };
}
