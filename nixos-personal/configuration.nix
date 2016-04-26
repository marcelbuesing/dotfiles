# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./scientific.nix
      #./npm-package.nix
    ];

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.grub.device = "/dev/sda";
  boot.tmpOnTmpfs = true;
  boot.devSize = "10%";

  # required for nvidia driver
  nixpkgs.config.allowUnfree = true;

  networking.hostName = "marcel-desktop"; # Define your hostname.
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ]; # Google DNS
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "de";
    defaultLocale = "de_DE.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "Europe/Amsterdam";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    cabal2nix
    cabal-install
    chromium
    dropbox
    emacs
    emacs24Packages.structuredHaskellMode
    ffmpeg
    firefox-wrapper
    freeglut
    gcc
    gcc_multi
    ghc
    ghostscript
    glmark2
    gimp
    git
    gnumake
    google-fonts
    hexchat
    irssi
    inkscape
    jdk
    keepassx2
    libpqxx
    mesa
    mesa_glu
    mplayer
    nix-repl
    npm2nix
    nodejs
    numix-icon-theme
    numix-icon-theme-circle
    paper-gtk-theme
    postgresql
    python
    ruby
    scribus
    skype
    spotify
    stack
    steam
    smplayer
    teamspeak_client
    tmux
    unrar
    vlc
    wireshark
    which
    wget
    zlib # required for postgresql
    zlibStatic
    zsh
    nodePackages.bower
    nodePackages.gulp
  ];

  programs.zsh.enable = true;

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts  # Micrsoft free fonts
      inconsolata  # monospaced
      ubuntu_font_family  # Ubuntu fonts
      unifont # some international languages
    ];
  };

  # List services that you want to enable:

  # Locate
  services.locate.enable = true;

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "us";
  services.xserver.xkbOptions = "eurosign:e";

  # Enable the KDE Desktop Environment.
  # services.xserver.displayManager.kdm.enable = true;
  # services.xserver.desktopManager.kde4.enable = true;

  services.xserver.videoDrivers = [ "nvidia" ];
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;

  # postgres
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql;
  services.postgresql.authentication = "local all all ident";

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.extraUsers.marcel = {
    isNormalUser = true;
    uid = 1000;
    home = "/home/marcel/";
    createHome = true;
    extraGroups = ["wheel"];
  };

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

  # Firewall
  networking.firewall.allowedTCPPorts = [8080];
  networking.firewall.allowedUDPPorts  = [10000]; # RTP IPTV
}
