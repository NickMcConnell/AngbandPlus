#include "constant.h"
#include "types.h"
#include "externs.h"

describe_mon_type desc_list[MAX_CREATURES] = {

{"Paavo Vaaranen", "It radiates an aura of deceit and unfair play"},
{"Fiona, the Sorceress of Amber","One of the most ruthless Amberites,"
    " with tremendous psionic powers"},
{"Benedict, the Ideal Warrior", "Reputedly the best warrior in the whole"
  " universe"},
{"Corwin, the Lord of Avalon","Corwin's skill and cunning are well known"},
{"Eric, the Usurper", "He once unrightfully grabbed the Crown of Amber"},
{"Gerard, the Strongman of Amber","The strongest guy there is"},
{"Caine, the Conspirator", "A real snake, one of the most crafty Amberites"},
{"Julian, the Master of Forest Arden","Morgentstern's mighty master"},
{"Bleys, the Trickster","He once even fooled Corwin"},
{"Blubbering idiot", "It tends to blubber a lot"},
{"Pitiful looking beggar", "You just can't help feeling sorry for it"},
{"Mangy looking leper", "You feel it isn't safe to touch it"},
{"Black Market deliverer", "Well, where did you think they get their"
    " wares? That Wand of Drain Life you bought had probably been"
    " stolen from a fellow adventurer of yours"},
{"Singing, happy drunk", "It makes you glad to be sober"},
{"Mean looking mercenary", "No job is too low for it"},
{"Battle scarred veteran", "It doesn't take to strangers kindly"},
{"Blinking dot", "It looks threatening"},
{"Giant yellow centipede", "It is about four-foot long and carnivorous"},
{"Millipede", "It is tiny, but carnivorous"},
{"Newt", "A very small lizard"},
{"Transparent jelly", "It is a smallish, slimy, icky, blobby creature"},
{"Greater hell-beast", "Your heart almost stops beating as you behold this"
    " undescribably horrendous creature. It approaches you, unstoppably,"
    " crushing everything that comes to its path. Flee! Flee its wrath while you can!"},
{"Large brown snake", "It is about eight-feet long"},
{"Dog from space", "It is an ordinary-looking dog that is wearing some"
    " really bizarre garments"},
{"Squire", "An aspiring knight, eager to gain glory in battle"},
{"White worm mass", "It is a large slimy pile of worms"},
{"Floating eye" , "A disembodied flotaing eye"},
{"Shrieker mushroom patch", "Yum! These look quite tasty"},
{"Huge drooling insect", "Just like in your worst nightmares: a drooling"
    " insectoid that hides under the bed during the daytime"},
{"Metallic green centipede", "It is about four-foot long and carnivorous"},
{"Novice warrior" , "He looks inexperienced but tough"},
{"Novice rogue" , "A rather shifty individual"},
{"Novice priest" , "He is tripping over his priestly robes"},
{"Novice mage" , "He is leaving behind a trail of dropped spell components"},
{"Yellow mushroom patch", "Yum! It looks quite tasty"},
{"White jelly" , "Its a large pile of white flesh"},
{"Stork" , "It wants to make its nest here"},
{"Giant black ant" , "It is about three-feet long"},
{"White harpy" , "A flying, screeching bird with a woman's face"},
{"Weirdo from another planet" , "A weird small slimy creature"},
{"Green worm mass" , "It is a large slimy pile of worms"},
{"Robot probe sent from Jupiter" , "It wants to analyze our food"},
{"Cave spider" , "A black spider that moves in fits and starts"},
{"Poltergeist" , "A ghastly, ghostly form"},
{"Metallic blue centipede", "It is about four-foot long and carnivorous"},
{"Cheerful leprechaun" , "It looks happy! Tiny, smiling fellow with a"
    " pipe and a hat"},
{"Black naga" , "A large black serpent's body with a female torso"},
{"Spotted mushroom patch", "Yum! It looks quite tasty"},
{"Giant owl" , "'Who!' Larger than a small eagle, this carnivorous beast"
    " welcomes you as its next meal"},
{"Ewok" , "Short little teddybear-like guys, full of merchandising "
    " possibilities. They don't really belong here"},
{"Giant white ant" , "It is about two-feet long and has sharp pincers"},
{"Yellow mold" , "It is a strange growth on the dungeon floor"},
{"My Little Pony", "So cute it makes you mad"},
{"Yellow worm mass" , "It is a large slimy pile of worms"},
{"Radiation eye" , "A glowing eye that seems to crackle with energy"},
{"Blue jelly" , "It's a large pile of pulsing blue flesh"},
{"Creeping copper coins" , "A strange pile of moving coins"},
{"Giant white rat" , "A very vicious rodent"},
{"Blue worm mass" , "A large slimy pile of worms"},
{"Large grey snake" , "It slithers and moves towards you"},
{"Wood spider" , "It creeps towards ou"},
{"Green naga" , "A large green serpent with a female's torso, the"
  " green skin glistens with acid"},
{"Green glutton ghost" , "It is a very ugly green ghost with a"
  " voracious appetite"},
{"Green jelly" , "It is a large pile of pulsing green flesh"},
{"Death sword" , "Beware! Animated by dark magic, these sentient swords"
    " lurk and prey on the unwary"},
{"Giddy goon" , "It is a smallish, slimy, icky creature that looks comical"},
{"Disenchanter eye" , "A large white floating eye that crackles with magic"},
{"Red worm mass" , "It is a large slimy pile of worms"},
{"Copperhead snake" , "It has a copper head and sharp venomous fangs"},
{"Purple mushroom patch" , "Yum! It looks quite tasty"},
{"Brown mold" , "A strange brown growth on the dungeon floor"},
{"Disembodied hand that strangled people" , "It screeches as it attacks"},
{"Creeping silver coins" , "A pile of silver coins that move on"
  " thousands of tiny legs"},
{"Snaga" , "It is one of the many weaker 'slave' orcs, often"
  " mistakenly known as a goblin"},
{"Cave orc" , "It is often found in huge numbers in deep caves"},
{"Hill orc" , "It is a hardy well-weathered survivor"},
{"Cute little dinosaur" , "They are so cute! And carnivorous"},
{"Giant white louse","Merely looking at it makes you itch."},
{"Manes" , "It is minor but aggressive demon"},
{"Bloodshot eye" , "A large floating bloodshot eye"},
{"Red naga" , "A large red snake with a woman's torso"},
{"Red jelly" , "It is a large pulsating mound of red flesh"},
{"Killer bee" , "It looks very poisonous"},
{"Crypt creep" , "Skeletal ghosts that look menacing"},
{"Rotting corpse" , "A disgusting sight, flesh falls off"
  " in large chunks as it shambles forward"},
{"Lost soul" , "It is almost insubstantial"},
{"Dirty cannibal" , "It looks hungry"},
{"Space monster" , "You can't see it"},
{"Ghast" , "Cave-dwelling undead monstrosities with a keen sense of smell"},
{"Gremlin" , "These annoying creatures can breed endlessly in the damp"
    " dungeons"},
{"Hunting hawk of Julian" , "It looks ferocious"},
{"Tax collector" , "He is after your cash"},
{"Yeti" , "A large white figure covered in shaggy fur"},
{"Goat" , "It is an animal with an attitude"},
{"Giant grey rat" , "It a rodent of unusual size"},
{"Black harpy" , "A woman's face on the body of a vicious black bird"},
{"Orc shaman" , "An orc dressed in skins who gestures wildly"},
{"Giant red ant" , "It is large and has venomous mandibles"},
{"King cobra" , "It is a large snake with a hooded face"},
{"Giant spider" , "It is a vast black spider whose bulbous body"
  " is bloated with poison"},
{"Quiver slot" , "A magically animated quiver slot, shooting arrows"},
{"Shadow creature of Fiona" , "They are capable of travelling through shadow"},
{"Undead mass" , "A sickening sight: pulsating mountain of flesh, hands,"
    " eyes, legs and other parts of human corpses. It seems to be growing"},
{"Disenchanter mold" , "It is a strange glowing growth on the dungeon floor"},
{"Tengu", "No other demon matches its teleporting mastery"},
{"Creeping gold coins" , "They are shambling forward on thousands of legs"},
{"Chaos tile" , "A floor tile corrupted by Chaos"},
{"Brigand" , "He is eyeing your backpack"},
{"Orc zombie" , "It is a shambling orcish corpse"},
{"Creeping mithril coins" , "They are shambling forward on needle sharp legs"},
{"Gnome mage" , "A mage of short stature"},
{"Black mamba" , "It has glistening black skin, a sleek body and"
  " highly venomous fangs"},
{"Grape jelly" , "A pulsing mound of glowing flesh"},
{"Care bear" , "A small humanoid that radiates some niceness"},
{"Priest" , "A robed humanoid dedicated to his god"},
{"Air spirit" , "A whirlwind of intelligent air"},
{"Skeleton human" , "It is animated"},
{"Zombie human" , "It is a shambling human corpse dropping chunks of"
  " flesh behind it"},
{"Moaning spirit" , "A ghostly apparition that shrieks horribly"},
{"Swordsman" , "A warrior of considerable skill"},
{"Fruminous bandersnatch" , "It looks like a vast armoured centipede with massive"
  " mandibles and a spiked tail"},
{"Killer brown beetle" , "It is a vicious insect with a tough carapace"},
{"Ogre" , "A hideous, smallish giant that is often found near or with orcs"},
{"Illusionist" , "A deceptive spell caster"},
{"Black orc" , "It is a large orc with powerful arms and deep black skin"},
{"Flying skull" , "Animated flying skulls, together they form a skullpack"},
{"Orc captain" , "An armoured orc with an air of authority"},
{"Software bug" , "Nasty little things - and they breed"},
{"Giant white dragon fly" , "It is a large fly that drips frost"},
{"Hill giant" , "A ten foot tall humanoid with powerful muscles"},
{"Greater wall monster" , "This mighty construct is bad news.."},
{"Malicious leprechaun" , "Unlike its benevolent cousin, it doesn't have"
    " the pipe. It grins evilly as it hurries to cause mischief"},
{"Guardian naga" , "A giant snake like figure with a woman's torso"},
{"Roadrunner" , "It looks like a huge purple ostrich. "
    "The fastest thing on the freeway"},
{"Frost giant" , "A twelve foot tall giant covered in furs"},
{"Spotted jelly" , "A jelly thing"},
{"Ochre jelly" , "A fast moving highly acidic jelly thing, that is eating"
  " away the floor it rests on"},
{"Griffon", "It is half lion, half eagle. It flies"
  " menacingly towards you"},
{"Dweller on the threshold" , "A magical ape-like guardian, known for its"
    " habits: eating magicians, and bad language"},
{"Mi-Go" , "Also called the Fungi from Yuggoth: 'They were pinkish things"
    " about five feet long; with crustaceous bodies bearing vast pairs of"
    " dorsal fins and membranous wings and several sets of articulate limbs,"
    " and with a sort of convuluted ellipsoid, covered with multitudes of"
    " very short antennae, where a head would ordinarily be...'"},
{"Umber hulk" , "It is like a huge beetle with glaring eyes and"
  " large mandibles capable of slicing through rock"},
{"Gelatinous cube" , "It is a strange vast gelatinous structure,"
  " that assumes cubic proportions as it lines all for walls of"
  " the corridors it patrols. Through its transparent jelly structure"
  " you can see treasures that has been engulfed, and a few corpses as well"},
{"Giant green dragon fly" , "A vast foul smelling dragonfly"},
{"Fire giant" , "A glowing fourteen foot tall giant, flames drip from its"
  " red skin"},
{"Quasit" , "The chaotic evil master's favourite pet"},
{"Deep one" , "'They were mostly shiny and slippery, but the ridges of their"
    " backs were scaly. Their forms vaguely suggested the anthropoid, while"
    " their heads were the heads of fish, with prodigious bulging eyes that"
    " never closed. At the sides of their necks were palpitating gills, and"
    " their long paws were webbed...'"},
{"Forest troll" , "Ugly, green-skinned"},
{"Water spirit" , "A whirlpool of sentient liquid"},
{"Giant brown scorpion" , "It is fast and poisonous"},
{"Earth spirit" , "It is humanoid in shape and made out of solid rock"},
{"Fire spirit" , "It is composed of pure flame"},
{"Uruk" , "An orc as large as a man, fears no sunlight"},
{"Stone giant" , "It is eighteen foot tall and looking at you"},
{"Stone golem" , "An animated statue"},
{"Red mold" , "Strange red growth on the dungeon floor; it seems"
  " to burn with flame"},
{"Quylthulg" , "Strange pulsing mound of flesh"},
{"Nexus quylthulg" , "A very unstable, strange pulsing mound of flesh"},
{"Chimera" , "It is a strange concoction of lion, dragon and goat. It"
  " looks very odd but very avoidable"},
{"Cloud giant" , "A twenty foot tall giant wreathed in clouds"},
{"Storm giant" , "A twenty-five foot tall giant wreathed in lighting"},
{"Blue dragon bat" , "It is a glowing blue bat with a sharp tail"},
{"Lesser wall monster" , "A bizarre construct of animated wall. It is constantly"
    " building new wall sections, using the existing walls as raw material"},
{"Killer stag beetle" , "It is a giant beetle with vicious claws"},
{"Autoroller" , "It looks like a huge, spiked roller-coaster"},
{"Giant yellow scorpion" , "It is a giant scorpion with a sharp stinger"},
{"Black ooze" , "It is a strangely moving puddle"},
{"Gug" , "A huge, furred, ape-like beast, which has two arms, both of which"
    " divide into two separate forearms and taloned paws. Its head is"
    " terrifying, divided by a vertical mouth, running from the top to"
    " the bottom. Its eyes are attached to its head by separate eyestalks"},
{"Red dragon bat" , "It is a sharp-tailed bat, wreathed in fire"},
{"Killer blue beetle" , "It is looking for prey"},
{"Kouko" , "It is a ghostly apparition with a humanoid form"},
{"Mummified human" , "It is a human form encased in mouldy wrappings"},
{"Banshee" , "It is a ghostly woman's form that wails mournfully"},
{"Stone troll" , "It is a giant troll with scabrous black skin"},
{"Killer red beetle" , "It is a giant beetle with poisonous mandibles"},
{"Triceratops" , "A prehistoric rhino"},
{"Agent of Benedict" , "Their master has sent them to spy on you"},
{"White wraith" , "It is a tangible but ghostly form made of white fog"},
{"Unicorn" , "It has been sent here by the great Unicorn of Order. It"
    " tries to stop you, as it thinks you may upset the balance of the"
    " universe even more than it is already"},
{"Vampire" , "You notice a sharp set of front teeth"},
{"Black knight" , "It is a figure encased in deep black plate armour;"
  " it looks at you menacingly"},
{"Mage" , "A mage of some power; you can tell by the size of his hat"},
{"Ice troll" , "It is a white troll with powerfully clawed hands"},
{"Dhole" , "Gigantic wormlike burrowing horrors that are covered in acid"},
{"Young blue dragon" , "Its"
  " still tender scales are a deep blue in hue; sparks crackle along its"
  " length"},
{"Young white dragon" , "Its"
  " still tender scales are a frosty white in hue; icy blasts of cold"
  " air come from it as it breathes"},
{"Young green dragon" , "Its"
  " still tender scales are a deep green in hue; foul gas seeps through"
  " its scales"},
{"Skeleton troll" , "It is a troll skeleton animated by dark dweomers"},
{"Giant static ant" , "It is a giant ant that crackles with energy"},
{"Manticore" , "It is a winged lion's body, with a human torso, and a"
  " tail covered in vicious spikes"},
{"Grave wight" , "It is a ghostly form with eyes that haunt you"},
{"Killer slicer beetle" , "It is a beetle with deadly sharp"
  " cutting mandibles and a rock-hard carapace"},
{"Ghost" , "You don't believe in them"},
{"Death watch beetle" , "It is a giant beetle that produces a chilling sound"},
{"Knight who says NI!" , "You can't say exactly why, but it looks frightening"},
{"Cave troll" , "It is a vicious monster, feared for its ferocity"},
{"Dimensional shambler" , "'...a thing not wholly ape, and not wholly insect."
    " Its hide hung loosely upon its frame, and its rugose, dead-eyed rudiment"
    " of a head swayed drunkenly from side to side. Its forepaws were"
    " extended, with talons spread wide, and its whole body was taught with"
    " murderous malignity despite its utter lack of facial description'"},
{"Teenage Mutant Ninja Turtle" , "A turtle moving on two legs,"
    " trained in deadly ninjitsu skills"},
{"Barrow wight" , "It is a ghostly nightmare of a entity"},
{"Giant skeleton troll" , "It is the animated form of a massive troll"},
{"Wolf" , "It howls and snaps at you"},
{"Irish wolfhound of Flora" , "It is a large watchdog with eyes full of cunning"},
{"Weir" , "It is a huge wolf with eyes that glow with manly intelligence"},
{"Multi-hued hound" , "Constantly shifting its colour, a large hound"
    " with a menacing outlook"},
{"Hell hound of Julian" , "It is a giant dog that glows with heat, flames pour"
  " from its nostrils"},
{"Water troll" , "It is a troll that reeks of brine"},
{"Olog" , "It is a massive intelligent troll with needle sharp fangs"},
{"Water elemental" , "It is a giant tempest of water"},
{"Fire elemental" , "It is a giant inferno of flames"},
{"Air elemental" , "It is a giant tornado of winds"},
{"Lich" , "It is a skeletal form dressed in robes. It radiates vastly"
  " evil power"},
{"Oriental vampire" , "Much more powerful than its Western counterpart; power"
  " eminates from its chilling frame"},
{"Giant red scorpion" , "It is a giant red scorpion. It looks poisonous"},
{"Earth elemental" , "It is a giant form composed of rock, with fists"
  " of awesome power"},
{"Whirling Eddie" , "It is a sentient whirlpool-looking elemental"},
{"Time elemental" , "You haven't seen it yet"},
{"Ooze elemental" , "Animated filth, an eyesore of ooze"},
{"Smoke elemental" , "Its blackened form crackles with heat"},
{"Young black dragon" , "Its"
  " still tender scales are a darkest black hue; acid drips from its body"},
{"Young red dragon" , "Its"
  " still tender scales are a deepest red hue; heat radiates from its"
  " form"},
{"Warlock" , "A gaunt figure, clothed in black robes"},
{"Mummified troll" , "It is a massive figure clothed in wrappings. You"
  " are wary of its massive fists"},
{"Cyclops" , "An evil giant, strong and stupid. It has only eye, but it really"
    " knows how to pack a punch"},
{"Mature white dragon" , "A large dragon, scales gleaming bright white"},
{"Xorn" , "A huge creature of the element Earth. Able to merge with its"
  " element, it has four huge arms protruding from its enormous torso"},
{"Grey wraith" , "A tangible but ghostly form, made of grey fog, the"
  " air around it feels deathly cold"},
{"Young multi-hued dragon" , "Beautiful scales of shimmering and magical"
    " colours cover it"},
{"Mature blue dragon" , "A large dragon, scales tinted deep blue"},
{"Mature green dragon" , "A large dragon, scales tinted deep green"},
{"Iridescent beetle" , "It is a giant beetle, whose carapace shimmers"
  " with vibrant energies"},
{"Vampire lord" , "A foul wind chills your bones as this ghastly"
  " figure approaches"},
{"Monastic lich" , "A skeletal form wrapped in monastic robes, this"
    " creature was once a monk of an evil sect, and has lost none of its"
    " martial prowess in the process of becoming a lich. It is also capable"
    " of highly destructive magic"},
{"Mature red dragon" , "A large dragon, scales tinted deep red"},
{"Mature black dragon" , "A large dragon, with scales of deepest black"},
{"Mature multi-hued dragon" , "A large dragon, scales shimmering many colours"},
{"Ancient blue dragon" , "A huge dragonic form, lightning crackles"
  " along its length"},
{"Emperor wight" , "Your life force is torn from your body as this"
  " powerful unearthly being approaches"},
{"Lloigor" , "In its natural form, it is a vortex of great power, but it"
    " usually assumes the form a serpent to attack its victims"},
{"Nether wraith" , "A form that hurts the eye, death permeates the"
  " air around it. As it nears you, a coldness saps your soul"},
{"Sorceror" , "A human figure in robes, he moves with magically"
  " improved speed, and his hands are ablur with spell casting"},
{"Ancient white dragon" , "A huge dragonic form, frost covers it from"
  " head to tail"},
{"Ancient green dragon" , "A huge dragonic form enveloped in clouds"
  " of poisonous vapour"},
{"Ancient black dragon" , "A huge dragonic form, pools of acid melt"
  " the floor around it"},
{"Disenchanter worm" , "A strange mass of squirming worms, magical"
  " energy crackles around this disgusting form"},
{"Rotting quylthulg" , "It is a pulsing flesh mound that reeks of death"
  " and putrefaction"},
{"Lesser titan" , "It is a humanoid figure thirty feet tall that gives"
  " off an aura of power and hate"},
{"Archon" , "Never a more heavenly being have you seen. The very holiness"
  " of its presence makes you deeply respect it. Few creatures can"
  " match the powers of an Archon, fewer still live to tell the tale"
  " after attacking one"},
{"Ancient red dragon" , "A huge dragonic form, wisps of smoke steam"
  " from its nostrils and the extreme heat surrounding it makes you"
  " gasp for breath"},
{"Death quasit" , "It is a demon of small stature, but its armoured"
  " frame moves with lightning speed and its powers make it a tornado"
  " of death and destruction"},
{"Ancient multi-hued dragon", "A huge dragonic form, many colours"
  " ripple down its massive frame. Few live to see another"},
{"Fire vampire" , "'...thousands of tiny points of light... The myriad points"
    " of light were living entities of flame! For wherever they touched, fire"
    " sprang up'"},
{"Clubber demon" , "It is a demon wildly swinging with two large clubs, one in"
  " each hand"},
{"Hunting horror" , "It is a large demon that looks like a winged serpent,"
    " with a horrible face"},
{"Dark young of Shub-Niggurath" , "It looks like an enormous writhing mass,"
    " formed out of ropy black tentacles, with goat-like hooves. Beneath the"
    " surface there are mouths dripping ooze"},
{"Byakhee" , "'They were not altogether crows, nor moles, nor buzzards, nor"
    " ants, nor decomposed human beings, but something I cannot and must not"
    " recall'"},
{"Hobbes the Tiger" , "You always thought it was a doll, alive only in Calvin's"
    " imagination. It appears you were wrong"},
{"Cacodemon" ,"It is a ball-like creature, with one main eye and"
  " twelve smaller eyes on stalks. It has thousands of fangs, beware!"},
{"Gobbledigook","He is a goblin of not so great power or cunning"},
{"Azog, King of the Uruk-Hai","He is also known as King of Khazad-dum."
  " His ego is renowned to be bigger than his head"},
{"Father Dagon" ,"A deep one of the largest size, their ruler, over 20 feet"
    " tall"},
{"Grendel" ,"A massive and cruel ogre of great power,"
  " drool slides caustically down his muscular frame. Despite his bulk,"
  " he strikes with stunning speed"},
{"Xaren" ,"It is a tougher relative of the Xorn, its hide glitters"
  " with metal ores"},
{"Minotaur" ,"It is a cross between a human and a bull"},
{"Formless spawn of Tsathogghua" ,"'...they were amorphous lumps of viscous"
    " black slime that took temporary shapes for various purposes'"},
{"Balrog" ,"It is a massive humanoid demon wreathed in flames"},
{"Vecna, the Emperor Lich" ,"He is a highly cunning, extremely"
  " magical being, spoken of in legends. This ancient shadow of death"
  " wilts any living thing it passes"},
{"Mickey Mouse", "The incarnation of commercialism, making money is his"
    " business"},
{"Great wyrm of power","The mightiest of all dragonkind, rarely seen in"
   " our plane existence. These dragons are sent on various missions"
   " in the Astral Plane and beyond, and are spoken of with reverence"},
{"Star-spawn of Cthulhu","Servitors of Great Cthulhu, these huge creatures"
    " have octopoid heads and leathery wings"},
{"The Amazing Spider-man","When a radioactive spider bit Peter Parker, he"
    " gained unusual powers. Now his stage has proceeded even further, he"
    " no longer resembles a human being - except for some annoying habits"},
{"Vlad Dracula, Prince of Darkness","The most feared of all vampires. He wears"
    " black garments and an aura of darkness and death surrounds him. His"
    " mesmerizing gaze is fixed on you..."},
{"Atlach-Nacha","This enormous, hideous spirit of void is in"
  " the form of a spider of immense proportions. It is one of the Great Old"
  " Ones, deadly and powerful. Its face looks somewhat human-like, with"
  " red eyes rimmed with hair"},
{"Smaug the Golden","Smaug is one of the Uruloki that still survive, a"
  " fire-drake of immense cunning and intelligence. His speed through"
  " air is matched by few other dragons and his dragon's fire is"
  " what legends are made of"},
{"Fafner the Dragon","One of the oldest dragons there are. It is rumored" 
   " that the sword Nothung is the only weapon capable of slaying this"
   " huge beast"},
{"Ancalagon the Black","'Rushing Jaws' is his name, and death is his"
  " game. No dragon of the brood of Glaurung can match him"},
{"The Norsa","Usually found in the deepest unexplored jungle, the Norsa"
    " is an elephant-like beast of legendary might. It has 5 trunks and each"
    " can breathe a different deadly element. The gaze of the Norsa's only eye"
    " causes fainthearts to die on the spot. Some people say one'd be safer"
    " with a thousand lions than with the Norsa - and not without a reason"},
{"Muar, the Balrog","A huge balrog, surrounded by raging pillars of"
  " fire, Muar is indeed a terrible opponent. Wielding a great whip of"
  " fire and blazing sword, his fury blisters your skin, melts your flesh"},
{"Surtur the Fire Demon","The ruler of fire demons, who are to invade"
  " Midgard and Asgard on the day of Ragnarok. Gigantic in size, surrounded"
  " by flickering flames, Surtur is a terrifying sight"},
{"Great Cthulhu","'...a monster of vaguely anthropoid outline, but with an"
    " octopus-like head whose face was a mass of feelers, a scaly,"
    " rubbery-looking body, prodigious claws on hind and fore feet, and"
    " long, narrow wings behind. This thing... was of a somewhat bloated"
    " corpulence... It lumbered slobberingly into sight and groping"
    " squeezed its gelatinous green immensity through the black"
    " ... a mountain walked or shambled'"},
{"Oberon, the Lord of Amber","You behold a mighty royal figure: the ruler of"
    " the Realm, the father of Amber. His whims are unexplainable and none"
    " dare question his will. Obviously, he has his plans concerning the"
    " future of shadow, and he does not much like meddlers, whom he considers"
    " you to be"},
{"The Serpent of Chaos","The Big Boss Himself. Before the universe, the"
    " Serpent was. The Unicorn of Order fought with the Serpent and stole"
    " one of its eyes. This eye is now known as the Jewel of Judgement,"
    " and with it, Dworkin Barimen drew the Pattern and thus gave birth to"
    " the infinite worlds of shadow. Now the Balance has been disrupted,"
    " the Pattern damaged, and all the shadow is being absorbed by the Serpent"
    " of Chaos. Unless it is stopped, the world as we know it will come to an"
    " end, all order reverting to Primal Chaos"},
{"The Incredible Hulk","A huge green hulk stands before you."
  " The walls and ceiling are reduced to rubble as the Incredible Hulk makes"
  " his way towards you. The creature has a somewhat limited vocabulary:"
  " 'HULK SMASH!'"},
{"Ariel, Queen of Air","A whirlwind of destruction, Ariel, air"
  " elemental sorceress avoids your blows with her extreme speed"},
{"Moire, Queen of Rebma","The Pattern is reflected in the sea below, and the"
    " underwater kingdom around it, Rebma, is ruled by this mysterious lady"},
{"Loge, Elemental Spirit of Fire","A raging pillar of fire, Loge burns"
    " every living thing beyond recognition. Gods in Valhalla trust him"
    " unaware of his traitorous nature: it is he who will burn Valhalla"
    " when Ragnarok comes"},
{"Mighty Mouse","Even without an 'S' on his shirt, he looks curiously familiar."
    " He flies in, wearing a cape, and shouts 'Here I come to save the day!'"},
{"The Phantom of the Opera","The man behind the mask. His cunning and cruelty"
    " are known, his name feared"},
{"The Gooey Kablooie","A great sentient jelly, omnivorous in nature"},
{"Bolg, Son of Azog","A large and powerful orc! He looks just like"
  " his daddy. He is tall and fast, but fortunately blessed"
  " with orcish brains"},
{"The Smithess","The animated statue of a smith. It strides towards you"
    " with deadly intents"},
{"Kharis the Mummy","The ancient, powerful mummy, having his vengeance"
    " on those who would desecrate his tomb"},
{"Fasolt the Giant","Big, brawny, powerful and stupid. While the description"
  " may fit all giants, Fasolt has an excess of those attributes"},
{"Freddy Krueger","He's your worst nightmare! This man, with a hat"
    " and a hideous face, has knives attached to his fingers"},
{"Morgenstern, Julian's steed" ,"Julian's mighty horse, molded out of shadow"
    " stuff by Julian. It is very aggressive in its behavior"},
{"Jurt, the Living Trump","A real snake, he was constantly trying to murder"
    " his throne to get better chances at power. He has undergone a treatment"
    " which has turned him to 'Living trump' - he can move from one place"
    " to another via an instant teleport"},
{"Mandor, Master of the Logrus","One of the greatest Logrus Masters ever,"
    " perhaps only the old Suhuy is better than Mandor. Mandor can easily"
    " destroy you with his powerful spells and the forces of chaos"},
{"Nazgul","A tall black cloaked figure, also known as a Ringwraith, with"
    " an uncanny hissing voice and fiery red eyes gazing at you from under"
    " his black hood. He longs to taste your blood"},
{"Strygalldwir","A demon from the Courts of Chaos. According to Corwin,"
    " 'it was well over six feet in height, with great branches of antlers"
    " growing out of its forehead. Nude, its flesh was a uniform ash-gray"
    " in color. It appeared to be sexless, and it had gray, leathery wings"
    " extending far out behind it'"},
{"Judge Fire","One of the four Dark Judges. A black skeleton, enveloped"
    " in flames, wielding a trident. Just like the rest of the Dark"
    " Judges, his only mission is to destroy all who live"},
{"Judge Mortis","Also a Dark Judge. Mortis has the power over all plagues"
    " and diseases. He looks like a humanoid zombie, with a skeletal cow"
    " head"},
{"Judge Fear","Another Dark Judge. It is said that whoever beholds his"
    " gaze is immediately slain by fear. And if a guy isn't, Judge Fear"
    " will make sure he won't get far anyway"},
{"Scatha the Worm","Scatha is an ancient and wise Dragon. Scatha has"
  " grown clever over the long years. His scales are covered with frost,"
  " and his breath sends a shower of ice into the air"},
{"Judge Death","The mightiest of the four Dark Judges. A grotesque parody"
    " of life - or death - a rotten, green, skeletal corpse with a sickening"
    " wide grin, as he speaks your sentence: '...the crime issss life..."
    " the ssssentenccce isss... DEATH!'"},
{"Mordred, Betrayer of King Arthur","A traitorous knight, once a Knight"
    " of the Round Table, but who turned renegade and eventually slew"
    " King Arthur in combat"},
{"Santa Claus","Santa is on his way to deliver all kinds of valuable"
    " gifts to nice people. Be nice and maybe you'll get a gift, too!"
    " Nobody should be nasty (or stupid) enough to attack Santa Claus"},
{"Master of the Universe","Nobody knows where they came from. These self-"
    "titled 'Masters of the Universe' are so powerful that they put titans"
    " to shame"},
{"Brand, Mad Visionary of Amber","Brand is a formidable foe. In his own"
    " way, he sees himself as a hero, the god creator and absolute monarch"
    " of his new universe to be. Unfortunately, he needs to destroy the"
    " existing one first before he can remake it in his own image. Brand is"
    " the one responsible for the damage to the Pattern and he is fully"
    " aware that your intention is to thwart him. You shiver as Brand"
    " prepares to dispatch you with his mighty psychic powers"},
{"Murazor, the Witch-King of Angmar","The Chief of the Ringwraiths. A"
  " fell being of devastating power. His spells are lethal and his"
  " combat blows crushingly hard. He moves at speed, and commands"
  " legions of evil to do his bidding. It is said that he is fated to never"
  " die by the hand of mortal man"},
{"Kashchei the Immortal","A stench of corruption and decay"
  " surrounds this undead prince, who has clearly risen from the grave"
  " to continue his foul plots and schemes"},
{"Master thief","Cool and confident, fast and lithe; protect"
  " your possessions quickly!"},
{"stairway to hell","Smoke and smell of brimstone are rising from there"},
{"Mithril golem","This is a statue of true-silver! Imagine how much"
  " that would be worth if you could take it home! "},
{"Eog golem","A deep grey statue, which is striding towards you with an"
  " all-too-familiar purpose. Your magic surprisingly feels much"
  " less powerful now"},
{"Colbran","A man-shaped form of living lightning, sparks and"
  " shocks crackle all over this madly capering figure. It leaps and"
  " whirls around and about you....."},
{"Silent Watcher","A stumpy figure carved from stone, a bird with many faces,"
  " with glittering eyes, this sentinel watches carefully your every move"},
{"Jabberwock","'Beware the Jabberwock, my son! / The jaws that bite, the"
  " claws that catch!' Run and run quickly, death incarnate chases behind you"},
{"Zog","The zogwarts (zog for short) are a loathsome race of extraterrestrial"
    " insectoids with hideous habits"},
{"Black ogre","A massive orc like figure, with black skin and powerful arms"},
{"Ghoul","A disgusting sight, it looks like a rotting corpse. These foul"
    " undead spirits roam the graveyards and prey on the living"},
{"Mumak","A massive elephant form with eyes twisted by madness"},
{"Tankero","A massive creature from some exotic country. Its form is hideous,"
    " and it has a trunk like that of an elephant in a pig-like face"},
{"Livingstone","These nasty beasts masquerade as wall sections"},
{"Elder thing","'They represented some ridged barrel-shaped object with thin"
    " horizontal arms radiating spoke-like from a central ring and with"
    " vertical knobs or bulbs projecting from the head and base of the barrel."
    " Each of these knobs was the hub of a system of five long, flat,"
    " triangular tapering arms arranged around it like the arms of a"
    " starfish...'"},
{"Hobo","Ugly doesn't begin to describe it"},
{"Spirit naga","A wraithly snakelike form with the torso of a"
  " beautiful woman, it is the most powerful of its kind"},
{"Angel","A lesser angel wearing little more than a loin cloth;"
  " its steely skin providing all the protection it needs"},
{"Archangel","A lesser angel protected by an aura of holiness."
  " Its muscular form looks extremely powerful next to your own frail body"},
{"Cherub","It is an angel moving very quickly, radiating holiness and"
  " capable of casting a volley of powerful spells in your direction"},
{"Seraph","It is an angel, fast and strong. You are stunned by"
  " its extreme holiness and try to resist all desires to obey it"},
{"Rock lizard","It is a small lizard with a hardened hide"},
{"Grid bug","A strange electric bug"},
{"Jackal","It is a yapping snarling dog, dangerous when in a pack"},
{"Grey mold","A small strange growth"},
{"Novice ranger","An agile hunter, ready and relaxed"},
{"Archpriest","An evil priest, dressed all in black. Deadly spells hit"
  " you at an alarming rate as his black spiked mace rains down blow"
  " after blow on your pitiful frame"},
{"Cave lizard","It is an armoured lizard with a powerful bite"},
{"Night lizard","It is a black lizard with overlapping scales and"
  " a powerful jaw"},
{"Death drake","It is a dragonlike form wrapped in darkness, you"
  " cannot make out its true form but you sense its evil"},
{"Dracolich","The skeletal form of a once great dragon, enchanted by"
  " magic most perilous, its animated form strikes with speed and"
  " drains life from its prey to satisfy its hunger"},
{"Shadow drake","It is a dragonlike form wrapped in shadow. Glowing"
  " red eyes shine out in the dark"},
{"Death knight","It is a humanoid form dressed in armour of an"
  " ancient form, from beneath its helmet, eyes glow a baleful red"
  " and seem to pierce you like lances of fire"},
{"Great storm wyrm","A vast dragon of power, storms and lightning"
  " crash around its titanic form. Deep blue scales reflect the"
  " flashes and highlight the creature's great muscles. It regards you"
  " with contempt"},
{"Great hell wyrm","A vast dragon of immense power. Fire leaps"
  " continuously from its huge form, the air around it scalds you."
  " Its slightest glance burns you, and you truly realize how"
  " insignificant you are"},
{"Great ice wyrm","An immense dragon capable of awesome destruction."
  " You have never felt such extreme cold, or witnessed such an icy"
  " stare. Begone quickly or feel its wrath"},
{"Shade","A shadowy form clutches at you from the darkness, a"
  " powerful undead with a deadly touch"},
{"Spectre","A phantasmal shrieking spirit, its wail drives the intense"
  " cold of pure evil deep within your body"},
{"Gloom","It is a form that screams its presence against the eye."
  " Death incarnate, its hideous black body seems to struggle"
  " against reality as the universe itself struggles to banish it"},
{"Grim reaper","It is an unlife of power almost unequaled. An affront"
  " to existence its very touch abuses and disrupts the flow of life"
  " and its unearthly limbs, of purest black, crush rock and flesh with ease"},
{"Dread","It is a form that screams its presence against the eye."
  " Death incarnate, its hideous black body seems to struggle"
  " against reality as the universe itself struggles to banish it"},
{"Shadowlord","It is a tall, black, cloaked figure. Red eyes are burning"
    " within the hook. A frightening aura of hatred, falsehood and cowardice"
    " surrounds you as the creature slides towards you, without a sound, as"
    " if in a dream"},
{"Demonist","A figure twisted by evil standing in robes of deepest crimson"},
{"Dark elf","An elven figure with jet black skin and white hair, its"
  " eyes are large and twisted with evil"},
{"Dark elven mage","A drow elven figure dressed all in black, hurling spells"
  " at you"},
{"Moon beast","'...great greyish-white slippery things which could expand"
    " and contract at will whose principal shape... was that of a sort of"
    " toad without any eyes, but with a curious vibrating mass of short pink"
    " tentacles on the end of its blunt, vague snout...'"},
{"Dark elven lord","A drow elven figure in armour and radiating evil power"},
{"Dark elven warrior","An drow elven figure in armour and ready with his sword"},
{"Shoggoth","'The nightmare, plastic column of fetid, black iridescence oozed"
    " tightly onward through its fifteen-foot sinus, gathering unholy speed"
    " and driving berofe it a spiral, re-thickening cloud of the pallid abyss"
    " vapor. It was a terrible... a shapeless converie of protoplasmic"
    " bubbles, faintly self-luminous, and with myriads of temporary eyes"
    " forming and unforming...'"},
{"Mongbat","Devilbats, notoriously difficult to kill. They travel in packs"
    " and take delight in devouring human flesh"},
{"Demonic quylthulg", "A pile of pulsing flesh that glows with an"
  " inner hellish fire, the world seems to cry out against it"},
{"Dragonic quylthulg", "It looks like it was once a dragon corpse,"
  " now deeply infected with magical bacteria that make it pulse in a"
  " foul and degrading way"},
{"Player ghost", "You don't believe what you are seeing"},
{"Aimless looking merchant","The typical ponce around town, with purse"
  " jingling, and looking for more amulets of adornment to buy"},
{"Raving lunatic","Drooling and comical"},
{"Green ooze","It is green and it's oozing"},
{"Smurf","If you are good enough, you may see one of these. They sing happily:"
    " 'La la, la la la...'"},
{"Soldier ant","A large ant with powerful mandibles"},
{"Predator","An alien life form, hunting for pleasure - you are its prey"},
{"Mind flayer","A humanoid form with a gruesome head, tentacular mouth"
  " and piercing eyes. Claws reach out for you and you feel a"
  " presence invade your mind"},
{"Headless","These horrors were created by a mutation which was twisted"
    " by dark sorcery. Their very presence strikes fear to the hearts"
    " of most travellers. Poison drips from their needlelike claws"},
{"Castle Amber guard","Well trained, deadly, out for your blood"},
{"Hand druj","A skeletal hand floating in the air, motionless except for"
  " its flexing fingers"},
{"Eye druj","A bloodshot eyeball floating in the air, you'd be forgiven"
  " for assuming it as harmless"},
{"Skull druj","A glowing skull possessed by sorcerous power, it need"
  " not move, but just blasts you with mighty magic"},
{"Young brass dragon", "It has a form that legends are made of. Its"
  " still tender scales are a tarnished gold hue, and light is"
  " reflected from its form"},
{"Young bronze dragon", "It has a form that legends are made of. Its"
  " still tender scales are a rich bronze hue, and its shape masks"
  " its true form"},
{"Mature brass dragon","A large Dragon, with scales of gleaming"
  " gold-like plating"},
{"Mature bronze dragon","A large Dragon, with scales of rich bronze"},
{"Ancient brass dragon","A huge Dragonic form, wreathed in a nimbus of light"},
{"Ancient bronze dragon","A huge Dragonic form, enveloped in a cascade"
  " of colour"},
{"Fire Angel","A dragonoid creature, captured in the Courts of Chaos, and"
    " trained to become a deadly assassin"},
{"Great crystal drake","A huge crystalline Dragon. Its claws could cut"
  " you to shreds and its teeth are razor sharp. Strange colours"
  " ripple through it as it moves in the light"},
{"High priest","A dark priest of the highest order. Powerful and evil,"
  " beware his many spells"},
{"Salamander","A black and yellow lizard.. WATCH OUT"},
{"Giant salamander","A large black and yellow lizard. You'd better run away"},
{"Clear worm mass","A disgusting mass of poisonous worms"},
{"Great wyrm of chaos","A massive Dragon of changing form. As you watch,"
  " it appears first fair and then foul. Its body is twisted by"
  " chaotic forces as it strives to stay real. Its very existence"
  " distorts the universe around it"},
{"Great wyrm of law","A massive Dragon of powerful intellect. It seeks"
  " to dominate the universe and despises all other life. It sees all"
  " who do not obey it as merely insects to be crushed underfoot"},
{"Giant tarantula","A giant spider with hairy black and red legs."},
{"Giant brass dragon fly","Large beating wings support this dazzling"
  " insect, as they beat a loud buzzing noise pervades the air"},
{"Giant bronze dragon fly","This vast gleaming bronze fly has wings"
  " which beat mesmerically fast"},
{"Giant black dragon fly","The size of a large bird, this fly drips"
  " caustic acid"},
{"Fruit bat","A fast-moving pest"},
{"Giant army ant","An armoured form moving with purpose, powerful on"
  " its own; flee when hordes of them march"},
{"Frankenstein's Monster","Tailored from the bodies of dead men, this unholy"
    " abomination - its maker's attempt to create life - is a mortal enemy to"
    " all who live"},
{"Light hound","A brilliant canine form whose light hurts your eyes,"
  " even at this distance"},
{"Shadow hound","A hole in the air in the shape of a huge hound. No"
  " light falls upon this form"},
{"Fire hound","Flames lick at its feet and its tongue is a blade of"
  " fire. You can feel a furnace heat radiating from the creature"},
{"Cold hound","A hound as tall as a man, this creature appears to"
  " be composed of angular planes of ice. Cold radiates from it and"
  " freezes your breath in the air"},
{"Energy hound","Saint Elmo's Fire forms a ghostly halo around this"
  " hound, and sparks sting your fingers as energy builds up in the"
  " air around you"},
{"Vibration hound","A blurry canine form which seems to be moving as"
  " fast as the eye can follow. You can feel the earth resonating"
  " beneath your feet"},
{"Water hound","Liquid footprints follow this hound as it pads around"
  " the dungeon. An acrid smell of acid rises from the dog's pelt"},
{"Air hound","Swirling vapours surround this beast as it floats"
  " towards you, seemingly walking on air. Noxious gases sting your throat"},
{"Earth hound","A beautiful crystalline shape does not disguise the"
  " danger this hound clearly presents. Your flesh tingles as"
  " it approaches..."},
{"Impact hound","A deep blue shape is visible before you, its canine"
  " form strikes you with an almost physical force. The dungeon"
  " floor buckles as if struck by a powerful blow as it stalks towards you"},
{"Inertia hound","Bizarrely, this hound seems to be hardly moving at"
  " all, yet it approaches you with deadly menace. It makes you tired"
  " just to look at it"},
{"Time hound","You get a terrible sense of deja vu, or is it a"
  " premonition? All at once you see a little puppy and a toothless"
  " old dog"},
{"Nether hound","You feel a soul-tearing chill upon viewing this beast,"
  " a ghostly shape of darkness. You think it may be a dog, but it"
  " makes you feel weaker just to look at it..."},
{"Nexus hound","A locus of conflicting points coalesce to form the"
  " vague shape of a huge hound. Or is it really there? Anyway, it seems"
  " to be coming towards you..."},
{"Plasma hound","The very air warps as pure elemental energy stalks"
  " towards you in the shape of a giant hound. Your hair stands on end"
  " and your palms itch as you sense trouble..."},
{"Hound of Tindalos","Swirling mass of elements, it is difficult to say if"
    " these creatures have a body at all. 'They are lean and athirst!'"},
{"Chaos hound","A constantly changing canine form, these hounds"
  " rush towards you as if expecting mayhem and chaos ahead. They appear"
  " to have an almost kamikaze relish for combat. You suspect all may"
  " not be as it seems..."},
{"Gravity hound","Unfettered by the usual constraints of gravity,"
  " these unnatural creatures are walking on the walls and even"
  " the ceiling! The earth suddenly feels rather less solid as you"
  " see gravity warp all round the monsters"},
{"Fire vortex","A whirling maelstrom of fire"},
{"Cold vortex","A twisting whirlpool of frost"},
{"Energy vortex","A shimmering tornado of air, sparks crackle along"
  " its length"},
{"Water vortex","A caustic spinning tower of water"},
{"Nexus vortex","A maelstrom of potent magical energy"},
{"Chaos vortex","Void, nothingness, spinning destructively"},
{"Aether vortex","An awesome vortex of pure magic, power radiates from its"
  " frame"},
{"Plasma vortex","A whirlpool of intense flame, charring the stones at"
  " your feet"},
{"Time vortex","You haven't seen it yet"},
{"Acidic cytoplasm","A disgusting animated blob of destruction, flee its"
  " gruesome hunger!"},
{"Black reaver","A humanoid form. Black as night. Advancing steadily"
  " and unstoppably. Flee"},
{"Great wyrm of balance","This massive Dragon is among the mightiest"
  " of Dragonkind. It is thousands of years old and seeks to maintain"
  " the Balance. It sees you as an upstart troublemaker without the"
  " wisdom to control your actions. It will destroy you"},
{"Ugluk, the Uruk","Another servant of Chaos, this Orc is"
  " strong and cunning. He is ugly and scarred from many power struggles"},
{"Omarax the Eye Tyrant","A vast baleful eye floating in the air. His"
  " gaze seems to shred your soul and his spells crush your will. He"
  " is ancient, his history steeped in forgotten evils, his"
  " atrocities numerous and sickening"},
{"Robin Hood, the Outlaw","This legendary archer steals from the rich (you"
   " qualify) and gives to the poor. Better watch your purse"},
{"Skeletor","The arch-enemy of He-Man, a mighty sorcerer."
    " His power is devastating"
  " and his speed unmatched in the underworld. Flee his wrath"},
{"Sphinx","Normally, a sphinx enjoys playing riddle games, but they are known"
   " to have other ways to win a reluctant creature for meal"},
{"Wyvern","The smaller cousin of the dragonkind, these carnivorous beasts"
    " move swiftly. A wyvern doesn't have the breath weapon of dragons, but"
    " its poisonous tail makes it a dangerous opponent nevertheless"},
{"Tyrannosaurus","A fast and strong carnivorous beast, towering high"
    " above you. You seem to be on its menu"},
{"Barney, the Dinosaur","A lovable purple reptilian. You can hear his"
    " singing: 'I love you, you love me...' For some reason, though,"
    " you feel aggravated at the creature and would do anything to kill"
    " it"},
{"Chthonian","A race of ancient, burrowing worm-like beings with tentacles"
    " surrounding their mouths"},
{"9-headed hydra","A strange reptilian hybrid with 9 smouldering heads"},
{"11-headed hydra","A strange reptilian hybrid with 11 smouldering heads"},
{"The Lernean Hydra","A massive legendary hydra. It has 12 powerful"
  " heads. Its many eyes stare at you as clouds of smoke and"
  " poisonous vapour rise from its seething form"},
{"Vampire bat","An undead bat that flies at your neck hungrily"},
{"Shimmering vortex","A strange pillar of shining light that hurts"
  " your eyes. Its shape changes constantly as it cuts through the"
  " air towards you. It is like a beacon, waking monsters from their slumber"},
{"Hellcat","'Their markings were precisely those of Siamese cats, only these"
    " were the size of tigers. Their eyes were of a solid, sun-bright yellow,"
    " pupilless'"},
{"Wereworm","A huge wormlike shape dripping acid, twisted by evil"
  " sorcery into a foul monster that breeds on death"},
{"Mirkwood spider","A strong and powerful spider from Mirkwood"
  " forest. Cunning and evil, it seeks to taste your juicy"
  " insides"},
{"Shadow","A mighty spirit of darkness of vaguely humanoid form."
  " Razor-edged claws reach out to end your life as it glides towards"
  " you, seeking to suck the energy from your soul to feed its power"},
{"Stupendous Man","Who IS that masked man, anyway?"},
{"Count Duckula","The undead duck, vampiric count, who nevertheless"
    " refuses to drink a drop of blood. He may have some other tricks"
    " up his sleeve, though"},
{"Alberich, the Nibelung King","Having renounced all love, he now lives"
  " only to acquire even more riches. With his magical Tarnhelm, Alberich,"
  " the greedy king of the night dwarfs, can move unseen or transport"
  " from one place to another at the speed of his thought"},
{"Novice mage","He is leaving behind a trail of dropped spell components"},
{"Gnome mage","A mage of short stature"},
{"Phantom","An unholy creature of darkness, the aura emanating from"
  " this evil being saps your very soul"},
{"Filthy street urchin","It looks squalid and thoroughly revolting"},
{"Carrion crawler","A hideous centipede covered in slime and with"
  " glowing tentacles around its head"},
{"Gargoyle","Demons with stone-like skin. Their origins are unknown,"
    " although they show some association with the Courts of Chaos."
    " They hunt in packs and appreciate the taste of human flesh"},
{"Blink dog","A strange magical member of the canine race, its form"
  " seems to shimmer and fade in front of your very eyes"},
{"Phase spider","A spider that never seems quite there. Everywhere you"
  " look it is just half-seen, in the corner of one eye"},
{"Gibbering mouther","A disgusting creature, fleshy mass with eyes and mouths"
    " that make blabbering noise"},
{"Basilisk","A creature reputedly so poisonous that it could kill from"
  " a distance. Its noxious presence makes you feel sick. Surely,"
  " whatever this unnatural beast touches must wither and die"},
{"Ethereal hound","A pale white hound. You can see straight through"
  " it. Pulsing red lines and strange fluorescent light hints at"
  " internal organs best left to the imagination"},
{"Novice archer","A nasty little fellow with a bow and arrow"},
{"Undead beholder","A huge eyeball that floats in the air. Black"
  " nether storms rage around its bloodshot pupil and light seems to"
  " bend as it sucks its power from the very air around it. Your"
  " soul chills as it drains your vitality for its evil enchantments"},
{"Will o' the wisp","A strange ball of glowing light. It disappears"
  " and reappears and seems to draw you to it. You seem somehow"
  " compelled to stand still and watch its strange dancing motion"},
{"Lobo","A sadistic, mad assassin, reputedly the best there is. He grins"
    " evilly as he proceeds to work on his latest victim: you! He really"
    " enjoys his business. Worst of all, the blood of Lobo, when spilled,"
    " will produce new Lobos.."},
{"Nightcrawler","This intensely evil creature bears the form of"
  " a gargantuan black worm. Its gaping maw is a void of blackness,"
  " acid drips from its steely hide. It is like nothing you have ever"
  " seen before, and a terrible chill runs down your spine as you face it..."},
{"Nightwalker","A huge giant garbed in black, more massive than a titan"
  " and as strong as a dragon. With terrible blows, it breaks your"
  " armour from your back, leaving you defenseless against its evil"
  " wrath. It can smell your fear, and you in turn smell the awful"
  " stench of death as this ghastly figure strides towards you menacingly"},
{"Death mold","It is the epitome of all that is evil, in a mold."
  " Its lifeless form draws power from sucking the souls of those"
  " that approach it, a nimbus of pure evil surrounds it. However... it"
  " can't move..."},
{"Pak, Master of Sinanju","Master of the legendary art of Sinanju, Pak"
    " could be described as the perfect killing machine. He moves so quickly"
    " that he is hard to spot with your bare eye. To save the people of his"
    " village - also called Sinanju - from starvation, he has to apply his"
    " incredible skills to gain any wealth acquirable"},
{"Dworkin Barimen","A creature that may once have been something"
    " human-like, now looks like a small hunchbacked gnome. He possesses"
    " powers whose nature and limits are unknown. Yet, once in his saner"
    " days, he inscribed the Pattern with the Eye of the Serpent, and"
    " thus, created our world"},
{"Dark and stormy knight","Legendary evil nobleman of immense might. It is"
    " said that no other knight can defeat him in combat. His armor is"
    " dark blue, and thunderclouds seem to surround his form"},
{"Lord of Chaos","It is one of the few true masters of the art,"
  " being extremely skillful in all forms of combat and, what's even"
  " more frightening, has mastered the forces of Chaos: he has the power"
  " to invoke raw Logrus"},
{"Logrus master","An adept at unarmed combat, the Logrus master strikes with"
  " stunning power. He can summon help from the Courts of Chaos and is able"
  " to shapechange his wounds to ease any pain"},
{"Novice priest","He is tripping over his priestly robes"},
{"Kid from space","They are incredibly ugly. You almost shriek in disgust when"
    " you see one"},
{"Nibelung","Nibelungs are a race of night dwarfs, ruled by the cruel tyrant"
    " Alberich, who has forged a supposedly all-powerful magic ring. They"
    " have come here fulfilling their master's orders: to gather him new"
    " riches"},
{"Novice ranger","An agile hunter, ready and relaxed"},
{"Jasra, Mistress of Brand","Brand's one-time mistress, a woman with a"
  " shocking appearance. Her scales rattle as she"
  " slithers towards you, venom dripping from her ghastly mouth."
  " Yet, this woman is mother to Rinaldo, Brand's son, who has the"
  " blood of Amber in his veins"},
{"Bat-Bat, the Dark Knight","Hero of the poor, oppressed and other bats."
    " He thinks you have stolen his ManMobile"},
{"Garfield","The fattest and most annoying cat that has ever lived. You feel"
    " aggravated at it"},
{"Mime, the Nibelung","Besides being a highly skilled smith, Mime is a"
     " very traitorous night dwarf"},
{"Hagen, Son of Alberich","Born of a woman bought by gold, Hagen is a creature"
    " of hate, just like his father. Unlike his father, he is as tall as a man"
    " and could pass for one"},
{"Abyss worm"," These horrible black creatures have found some strange"
    " way out of the Abyss they belong to. Their numbers seem to increase,"
    " eating everything, the floor, the air, you..."},
{"Calvin, the horror kid from hell","He's a royal pain, a real nuisance"},
{"Gargamel, the Evil Wizard","He hates smurfs! Even though you don't look"
    " like a smurf, he is nasty enough to attack you, too"},
{"Lord Borel of Hendrake","Lord Borel is of the House of Hendrake, one"
  " of the migthiest noble families in the Courts of Chaos. His prowess"
  " at arms is so great that not even Corwin of Amber dared take on him in"
  " a fair fight"},
{"Lockheed, the cute little dragon","Old, even though not bigger than a cat"
    " in size. Beware its fiery breath"},
{"Astral hound","A pale white hound. You can see straight through"
  " it. Pulsing red lines and strange fluorescent light hints at"
  " internal organs best left to the imagination"},
{"Enchantress","This elusive female spellcaster has a special affinity"
  " for dragons, whom she rarely fights without"},
{"Dark elven druid","A powerful drow, with mighty nature"
  " controlling enchantments"},
{"Sasquatch","A tall shaggy, furry humanoid, he could call the"
  " Yeti brother"},
{"Raal's Tome of Destruction","Powerful arcane tome of spells, now its sorcery"
    " has been twisted out of control and it casts its deadly spells with"
    " malevolent intent"},
{"Drolem","A constructed dragon, the Drolem has massive strength."
  " Powerful spells weaved during its creation make it a"
  " fearsome adversary. Its eyes show little intelligence, but it has"
  " been instructed to destroy all it meets"},
{"Klingsor, the Evil Master of Magic","Klingsor is a mighty spell caster,"
  " his power and evil are undeniable. Once, he even stole the Holy Spear"
  " of the Grail - the Spear of Destiny - and wounded the King of the Grail"
  " with it. Normally, Klingsor dwells in a castle with a magical garden"
  " he has conjured up in the middle of a desert"},
{"Vorpal bunny","White, deceivingly pretty-looking small rabbit, that has"
    " deadly fangs and a tendency to go for the victim's throat... it is"
    " carnivorous and deadly"},
{"Magic mushroom patch","Yum! It looks quite tasty.  But, wait... It seems"
  " to glow with an unusual light"},
{"Ettin","A massive troll of huge strength. Ettins are stupid but violent"},
{"Anti-paladin","Cowardly, traitorous, dishonest, proud... totally opposite"
    " to the high ideals paladins represent. This sworn enemy of honorable"
    " knights is clad in black, and he takes delight in maiming and torturing"
    " the innocent and even his own henchmen"},
{"The Queen Ant","She's upset because you hurt her children"},
{"X-man","A mutant. Only 1% of all mutants survive their mutations. Only 1% of"
    " these develop weird talents. But in this game, just like in certain"
    " comic books, all mutants survive their mutations AND develop superhuman"
    " powers"},
{"Model T-1000 Terminator","A bizarre killing machine from the future,"
    " programmed to destroy you"},
{"Chaos shapechanger","A creature from the Courts of Chaos, constantly"
    " changing its appearance. It looks weird"},
{"Shantak","'They were not any birds or bats known elsewhere on earth... for"
    " they were larger than elephants and had heads like a horse's... The"
    " Shantak-bird has scales instead of feathers...'"},
{"Kavlax the Many-Headed","A Large dragon with a selection of heads,"
  " all shouting and arguing as they look for prey, but each with its"
  " own deadly breath weapon"},
{"Baphomet the Minotaur Lord","A fearsome bull-headed demon,"
  " Baphomet swings a mighty axe as he curses all that defy him"},
{"Spectral tyrannosaur","An evil undead tyrannosaur, its green luminous form"
    " radiates death"},
{"Shambling mound","A pile of rotting vegetation that slides towards"
  " you with a disgusting stench, waking all it nears"},
{"Spaceman Spiff","The daring star pilot waves his blaster menacingly at"
    " you!"},
{"Cerberus, Guardian of Hades","A 2 Headed hell hound of fearsome"
  " aspect. Flame burns merrily from its hide as it snarls and roars"
  " its defiance"},
{"It","The very presence of this unknown menace fills your heart with"
    " an uncanny sense of awe. Nobody has ever seen It"},
{"Mean Machine","The most feared member of the legendary Angel gang. He has"
    " a dial on his forehead. It seems to be stuck at 4,5"},
{"Druid","A mystic being at one with nature."},
{"Pseudo dragon","A small relative of the dragon that inhabits dark caves"},
{"Chaos drake","A dragon twisted by the forces of chaos. It seems"
  " first ugly, then fair, as its form shimmers and changes in front"
  " of your eyes"},
{"Law drake","This dragon is clever and cunning. It laughs at your"
  " puny efforts to disturb it"},
{"Balance drake","A mighty dragon, the Balance drake seeks to maintain"
  " the Balance, and despises your feeble efforts to destroy evil"},
{"Baby white dragon","This hatchling dragon is still soft, its"
  " eyes unaccustomed to light and its scales a pale white"},
{"Baby blue dragon","This hatchling dragon is still soft, its"
  " eyes unaccustomed to light and its scales a pale blue"},
{"Baby black dragon","This hatchling dragon is still soft, its"
  " eyes unaccustomed to light and its scales a dull black"},
{"Baby green dragon","This hatchling dragon is still soft, its"
  " eyes unaccustomed to light and its scales a sickly green"},
{"Baby red dragon","This hatchling dragon is still soft, its"
  " eyes unaccustomed to light and its scales a pale red"},
{"Baby multi-hued dragon","This hatchling dragon is still soft, its"
  " eyes unaccustomed to light and its scales shimmering with a hint"
  " of colour"},
{"Rinaldo, Son of Brand","He is both skilled and dangerous; he is known"
    " for his relentless attempts at assassination. He has his father's"
    " cunning"},
{"Creeping adamantite coins","A mass of shining coins slithering"
  " towards you.. Quick! Pick it up and put it in your pocket"},
{"Black pudding","A lump of rotting black flesh that slurrrrrrrps"
  " across the dungeon floor"},
{"Silver jelly","It is a large pile of silver flesh that sucks all light"
  " from its surroundings"},
{"Uriel, Angel of Fire","A creature of godly appearance, you dare"
  " not challenge Uriel's supremacy. Those who stood against him before"
  " are but a memory, cremated by his mastery of elemental"
  " fire"},
{"Azriel, Angel of Death","Azriel commands awesome power, his visage"
  " holy enough to shrivel your soul. You shriek with disbelief as"
  " his mastery of death draws you to your grave. It is truly beyond"
  " all but the mightiest of warriors to stand against him and live"},
{"Raphael, the Messenger","Commanding a legion of Archons, Raphael"
  " will destroy you for your sins. He will crush you like the"
  " pitiful insignificant being he sees you to be. Your very soul"
  " will be taken into judgement by his supreme authority, as he cleanses the"
  " world of evil"},
{"Oriental dragon","A huge Dragon emanating from the elemental planes,"
  " the Oriental dragon is a master of light and dark. Its form"
  " disappears from sight as it cloaks itself in unearthly shadows"},
{"Ethereal drake","A dragon of elemental power, with control over light"
  " and dark, the Ethereal drake's eyes glare with white hatred from"
  " the shadows"},
{"Tselakus, the Dreadlord","This huge affront to existence twists and"
  " tears at the fabric of space. A master of mighty magic,"
  " Tselakus hungers for your tender flesh. Darkness itself recoils"
  " from the touch of Tselakus as he leaves a trail of death"
  " and destruction. Tselakus is a being of sneering"
  " contempt, laughing at your pitiful efforts to defy him. Mighty"
  " claws rend reality as he annihilates all in his path to your soul"},
{"Ogre mage","A hideous ogre wrapped in black sorcerous robes"},
{"Greater titan","A forty foot tall humanoid that shakes the ground as"
  " it walks. The power radiating from its frame shakes your courage,"
  " its hatred inspired by your defiance"},
{"Night mare","A fearsome skeletal horse with glowing eyes, that watch"
  " you with little more than a hatred of all that lives"},
{"Groo the Wanderer","Unsurpassably stupid and ugly, Groo is a formidable"
    " fighter nonetheless. He is known for taking out entire armies all"
    " by himself. Nobody feels safe when Groo is around"},
{"Godzilla","It rose from the contaminated sea. Covered with toxic wastes,"
    " its very breath is death. Destroyer of cities, rumored to be"
    " unstoppable and immune to all magic"},
{"The Lizard King","A nasty piece of work, the Lizard King recognizes no such"
    " thing as a fair fight"},
{"Master quylthulg","A pulsating mound of flesh, shining with silver"
  " pulses of throbbing light"},
{"Greater dragonic quylthulg","A massive mound of scaled flesh,"
  " throbbing and pulsating with multi-hued light"},
{"Greater rotting quylthulg","A massive pile of rotting flesh. A"
  " disgusting stench fills the air as it throbs and writhes"},
{"The Emperor Quylthulg","A huge seething mass of flesh with a"
  " rudimentary intelligence, the Emperor Quylthulg changes colours"
  " in front of your eyes. Pulsating first one colour then the next,"
  " it knows only it must bring help to protect itself"},
{"Qlzqqlzuup, the Lord of Flesh","This disgusting creature squeals"
  " and snorts as it writhes on the floor. It pulsates with evil,"
  " its intent to overwhelm you with monster after monster, until it"
  " can greedily dine on the remains"},
{"Mimic","A strange creature that disguises itself as discarded objects"
  " to lure unsuspecting adventurers within reach of its venomous claws"},
{"Pole axe of animated attack","An animated pole axe weapon which relentlessly"
    " attacks all intruders"},
{"Mimic","A strange creature that disguises itself as discarded objects"
  " to lure unsuspecting adventurers within reach of its venomous claws"},
{"Scrawny cat","A skinny little furball with sharp claws and a menacing look"},
{"Scruffy little dog","A thin flea-ridden mutt, growling as you get close"},
{"Pink panther","An elegant-looking panther, with an intelligent gaze in its"
    " eyes"},
{"Panther","A large black cat, stalking you with intent. It thinks you"
  "'re its next meal"},
{"Tiger","One of the largest of its species, a sleek orange and black"
  " shape creeps towards you, ready to pounce"},
{"Sabre-tooth tiger","A fierce and dangerous cat, its huge tusks and"
  " sharp claws would lacerate even the strongest armour"},
{"Martti Ihrasaari","He is the president in some remote country."
    " He weighs 127 kg, and looks suspicious"},
{"Cyberdemon", "Heavy metal steps announce the approach of this huge"
   " demon"},
{"Displacer beast","It is a huge black panther, clubbed tentacles"
  " sprouting from its shoulders"},
{"Spectator","A creature with small eyestalks around a larger central eye"},
{"Lurker","A strange creature that merges with the dungeon floor"},
{"Trapper","A larger cousin of the lurker, this creature traps"
  " unsuspecting victims and paralyzes them, to be slowly digested later"},
{"He-Man","The mightiest of all Masters of the Universe, the Ruler of the"
    " Castle Grayskull! You can hear his war-cry: 'I... have... the POWER!'"},
{"Giant roc","A vast legendary bird, its iron talons rake the"
  " most impenetrable of surfaces and its screech echoes through the"
  " many winding dungeon corridors"},
{"The Phoenix","A massive glowing eagle bathed in flames"},
{"Ithaqua the Windwalker","Also known as the Wendigo, this horrid creature"
    " moves so quickly that a normal eye cannot catch him. His howling haunts"
    " the northern icy wastelands"},
{"Novice paladin","He seems to consider you an agent of the devil"},
{"Professor X","The leader and founder of the X-men, a mutant with exceptional"
    " psychic powers"},
{"Great wyrm of many colors","The mightiest of all multi-hued dragons"},
{"$crooge McDuck","The richest duck there is"},
{"Baron of hell","Horn-headed, nicknamed 'goo-thrower'. Hard to kill"},
};
