module internal NightLight.Core.Configuration

open NightLight.Core.Moods
open NightLight.Core.Lights

let getDesiredColorAndBrightness bulb mood =
    let white = ColorByCoordinates(0.3227, 0.329)
    let yellow = ColorByTemperature 454
    let red = ColorByCoordinates(0.6942, 0.2963)

    match bulb, mood with
    | IkeaBulb, White -> white, Brightness 254
    | IkeaBulb, Yellow -> yellow, Brightness 210
    | IkeaBulb, Red -> red, Brightness 254
    | PaulmannBulb, White -> white, Brightness 35
    | PaulmannBulb, Yellow -> yellow, Brightness 35
    | PaulmannBulb, Red -> red, Brightness 80

let lights =
    [ { FriendlyName = "Vardagsrum - Fönsterlampa"
        Room = LivingRoom
        Bulb = IkeaBulb }
      { FriendlyName = "Vardagsrum - Vägglampa"
        Room = LivingRoom
        Bulb = PaulmannBulb }
      { FriendlyName = "Vardagsrum - Golvlampa"
        Room = LivingRoom
        Bulb = PaulmannBulb }
      { FriendlyName = "Badrum - Taklampa"
        Room = Bathroom
        Bulb = IkeaBulb }
      { FriendlyName = "Sovrum - Nattduksbordlampa"
        Room = Bedroom
        Bulb = IkeaBulb } ]
