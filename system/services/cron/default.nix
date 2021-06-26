{ config, pkgs, ... }:

# Enable cron service
{
  services.cron = {
    enable = true;
    systemCronJobs = [
      "*/5 * * * *      ben    ${pkgs.notmuch}/bin/notmuch new &>> /tmp/cron.notmuchmail.log"
    ];
  };
}
