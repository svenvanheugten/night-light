self:
{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
let
  inherit (pkgs.stdenv.hostPlatform) system;
  cfg = config.services.nightLight;
in
{
  options.services.nightLight = {
    enable = mkEnableOption "Night Light";
  };

  config = mkIf cfg.enable {
    services.zigbee2mqtt.enable = true;
    services.mosquitto.enable = true;
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
        ExecStart = "${self.packages.${system}.default}/bin/NightLight";
        Restart = "on-failure";
        User = "night-light";
        Group = "night-light";
      };
    };
  };
}
