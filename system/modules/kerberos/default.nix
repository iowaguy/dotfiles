{
  krb5 = {
    enable = true;
    # forwardable tickets necessary to do things like access AFS without
    # re-`kinit`-ing (you want this)
    libdefaults = { forwardable = true; };
  };
}
