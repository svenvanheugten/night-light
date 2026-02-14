{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  cfg = config.services.nightLight;
  pkg = pkgs.callPackage ./default.nix { };
in
{
  options.services.nightLight = {
    enable = mkEnableOption "Night Light";
  };

  config = mkIf cfg.enable {
    services.zigbee2mqtt.enable = true;
    users.groups.night-light = { };
    users.users.night-light = {
      isSystemUser = true;
      description = "Night Light";
      group = "night-light";
    };
    systemd.services.night-light = {
      description = "Night Light";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        ExecStart = "${pkg}/bin/NightLight";
        Restart = "on-failure";
        User = "night-light";
        Group = "night-light";
      };
    };
  };
}
