{ ... }:
{
  users.groups = {
    wheel.gid = 1;
    networkmanager.gid = 57;
    libvirtd.gid = 67;
    docker.gid = 131;
    qemu-libvirtd.gid = 301;
    dhcpcd.gid = 992;
    fwupd-refresh.gid = 982;
    geoclue.gid = 999;
    nscd.gid = 990;
    polkituser.gid = 997;
    sshd.gid = 995;
    systemd-coredump.gid = 994;
    systemd-oom.gid = 989;
  };
}
