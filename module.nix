{
  config,
  lib,
  ...
}:

with lib;
let
  cfg = config.services.nightLight;
in
{
  options.services.nightLight = {
    enable = mkEnableOption "Night Light";
  };

  config = mkIf cfg.enable {
    services.zigbee2mqtt.enable = true;
  };
}
