module internal NightLight.Core.Lights

open NightLight.Core.Moods

type Bulb =
    | IkeaBulb
    | PaulmannBulb

type Color =
    | ColorByCoordinates of float * float
    | ColorByTemperature of int

type Brightness = Brightness of int

type Light =
    { FriendlyName: string
      Room: Room
      Bulb: Bulb }
