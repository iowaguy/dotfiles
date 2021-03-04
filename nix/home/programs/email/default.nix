{ pkgs, ... }:
{
  home.file.".config/afew/config".source = ./afew-config.ini;
  home.file.".notmuch-config".source = ./notmuch-config.ini;

  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "${pkgs.isync}/bin/mbsync --all";
      postNew = ''
        ${pkgs.afew}/bin/afew --tag --new
        notmuch tag +newyorker -inbox -- "from:newyorker@newsletter.newyorker.com"
        notmuch tag +scholarly-reading -inbox -- "from:scholaralerts-noreply@google.com"
        notmuch tag +lightning-dev -inbox -- "from:lightning-dev-request@lists.linuxfoundation.org"
        notmuch tag +calnewport -inbox -- "from:author@calnewport.com"
        notmuch tag +economist -inbox -- "from:newsletters@e.economist.com"
      '';
    };
  };
  accounts.email = {
    accounts = {
      gmail = {
        address = builtins.concatStringsSep "@" [ "benweintraub34" "gmail.com" ];
        imap.host = "imap.gmail.com";
        mbsync = {
          enable = true;
          create = "maildir";
        };
        msmtp.enable = true;
        notmuch.enable = true;
        primary = true;
        realName = "Ben Weintraub";
        signature = {
          text = ''
            --
            Ben Weintraub
            PhD Student
            Khoury College of Computer Sciences
            Northeastern University
            https://ben-weintraub.com/
          '';
          showSignature = "append";
        };
        passwordCommand = "cat $HOME/Dropbox/.secrets/mbsync-pass.txt";
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

        imap.port = 993;
        maildir.path = "gmail";
      };
    };
  };
}
