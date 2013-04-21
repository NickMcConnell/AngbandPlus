/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var events = new Object();
  
  events["Fast"] = { 
    "display" : "hastened",
    "beginMessage" : "You feel time slowing down!",
    "endMessage" : "You regain normal speed"
  };

  events["Slow"] = { 
    "display" : "slowed",
    "beginMessage" : "You feel yourself moving slower!",
    "endMessage" : "You feel yourself speed up."
  };

  events["Blind"] = { 
    "display" : "blind",
    "beginMessage" : "You are blind!",
    "endMessage" : "You can see again."
  };

  events["Paralyzed"] = { 
    "display" : "paralyzed",
    "beginMessage" : "You are paralyzed!",
    "endMessage" : "You can move again."
  };

  events["Confused"] = { 
    "display" : "confused",
    "beginMessage" : "You are confused!",
    "endMessage" : "You feel less confused now."
  };

  events["Afraid"] = { 
    "display" : "terrified",
    "beginMessage" : "You are terrified!",
    "endMessage" : "You feel bolder now."
  };

  events["Hallucinating"] = { 
    "display" : "hallucinating",
    "beginMessage" : "Oh	wow! Everything looks so cosmic now!",
    "endMessage" : "You can see clearly again."
  };

  events["Poisoned"] = { 
    "display" : "poisoned",
    "beginMessage" : "You are poisoned!",
    "endMessage" : "You are no longer poisoned."
  };

  events["Cut"] = { 
    "display" : "cut",
    "beginMessage" : "You are cut!",
    "endMessage" : "You are no longer bleeding."
  };

  events["Stun"] = { 
    "display" : "stunned",
    "beginMessage" : "You are stunned!",
    "endMessage" : "You are no longer stunned."
  };

  events["Protected From Evil"] = { 
    "display" : "protected from evil",
    "beginMessage" : "You feel safe from evil!",
    "endMessage" : "You no longer feel safe from evil."
  };

  events["Invulnerable"] = { 
    "display" : "invulnerable",
    "beginMessage" : "Invulnerability!",
    "endMessage" : "The invulnerability wears off."
  };

  events["Heroic"] = { 
    "display" : "heroic",
    "beginMessage" : "You feel like a hero!",
    "endMessage" : "The heroism wears off."
  };

  events["Super Heroic"] = { 
    "display" : "raging",
    "beginMessage" : "You feel the surge of a cold rage!",
    "endMessage" : "Your rage wears down."
  };

  events["Shielded"] = { 
    "display" : "shielded",
    "beginMessage" : "Your are shielded.",
    "endMessage" : "Your shield dissipates."
  };

  events["Blessed"] = { 
    "display" : "blessed",
    "beginMessage" : "You feel righteous!",
    "endMessage" : "The prayer has expired."
  };

  events["Seeing Invisible"] = { 
    "display" : "seeing invisible",
    "beginMessage" : "Your eyes feel very sensitive!",
    "endMessage" : "Your eyes feel less sensitive."
  };

  events["Seeing Infrared"] = { 
    "display" : "seeing infra-red",
    "beginMessage" : "Your eyes begin to tingle!",
    "endMessage" : "Your eyes stop tingling."
  };

  events["Occluded"] = { 
    "display" : "anti-magical",
    "beginMessage" : "You feel shielded from magic.",
    "endMessage" : "You no longer feel shielded from magic."
  };

  events["Acid Resistant"] = { 
    "display" : "resistant to acid",
    "beginMessage" : "You feel resistant to acid!",
    "endMessage" : "You feel less resistant to acid."
  };

  events["Electricity Resistant"] = { 
    "display" : "resistant to electricity",
    "beginMessage" : "You feel resistant to electricity!",
    "endMessage" : "You feel less resistant to electricity."
  };

  events["Fire Resistant"] = { 
    "display" : "resistant to fire",
    "beginMessage" : "You feel resistant to fire!",
    "endMessage" : "You feel less resistant to fire."
  };

  events["Cold Resistant"] = { 
    "display" : "resistant to cold",
    "beginMessage" : "You feel resistant to cold!",
    "endMessage" : "You feel less resistant to cold."
  };

  events["Poison Resistant"] = { 
    "display" : "resistant to poison",
    "beginMessage" : "You feel resistant to poison!",
    "endMessage" : "You feel less resistant to poison."
  };

  events["Telepathic"] = { 
    "display" : "psychic",
    "beginMessage" : "You feel your consciousness expand!",
    "endMessage" : "Your consciousness contracts again."
  };

  events["Wraith"] = { 
    "display" : "incorporeal",
    "beginMessage" : "You become a ghastly wraith-being!",
    "endMessage" : "You are no longer a wraith."
  };

