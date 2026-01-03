module internal NightLight.Core.Moods

open NightLight.Core.PartsOfDay

type Mood =
    | White
    | Yellow
    | Red

type Room =
    | Bathroom
    | LivingRoom
    | Bedroom

let getDesiredMood room partOfDay =
    match room, partOfDay with
    | Bathroom, Day -> White
    | LivingRoom, Day -> Yellow
    | Bedroom, Day -> Yellow
    | _, Night -> Red
