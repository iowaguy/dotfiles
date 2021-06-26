{ pkgs, ... }:

{

  home.file.".mailcap".text = ''
    #image/gif; open %s;
    #image/*; open %s;

    image/pdf; evince %s
    application/pdf; evince %s;

    #text/html; open '%s'\; sleep 1; needsterminal;
    #text/html; w3m -I %{charset} -T text/html; copiousoutput;

    #text/plain; ~/.bin/em %s;
    #text/x-tex; ~/.bin/em %s;

    video/*; vlc %s;

    #application/vnd.openxmlformats-officedocument.spreadsheetml.sheet; open %s;
    #application/vnd.openxmlformats-officedocument.wordprocessingml.document; open %s;
  '';
}
