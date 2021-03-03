{ pkgs, ... }:
{
  programs.mbsync.enable = true;
  programs.msmtp.enable = true;
  programs.notmuch = {
    enable = true;
    hooks = {
      preNew = "mbsync --all";
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
