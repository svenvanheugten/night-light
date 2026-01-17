module internal NightLight.Core.Moods

open NightLight.Core.PartsOfDay
open NightLight.Core.Models

type Mood =
    | White
    | Yellow
    | Red

let getDesiredMood room partOfDay =
    match room, partOfDay with
    | Bathroom, Day -> White
    | LivingRoom, Day -> Yellow
    | Bedroom, Day -> Yellow
    | _, Night -> Red

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
