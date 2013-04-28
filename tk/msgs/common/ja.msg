set loc ja

# This is the Angband(name) variable used for window titles etc
::msgcat::mcset $loc AngbandTk
::msgcat::mcset $loc KAngbandTk
::msgcat::mcset $loc OAngbandTk
::msgcat::mcset $loc ZAngbandTk

::msgcat::mcset $loc Browse... 参照...
::msgcat::mcset $loc Cancel キャンセル
::msgcat::mcset $loc Close 閉じる
::msgcat::mcset $loc Find 検索
::msgcat::mcset $loc Find... 検索
::msgcat::mcset $loc "Find Again" 再検索
::msgcat::mcset $loc OK
::msgcat::mcset $loc "Set Font"
::msgcat::mcset $loc Yes
::msgcat::mcset $loc No
::msgcat::mcset $loc Accept
::msgcat::mcset $loc All

if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
::msgcat::mcset $loc Human 人間
::msgcat::mcset $loc Half-Elf ハーフエルフ
::msgcat::mcset $loc Elf エルフ
::msgcat::mcset $loc Hobbit ホビット
::msgcat::mcset $loc Gnome ノーム
::msgcat::mcset $loc Dwarf ドワーフ
::msgcat::mcset $loc Half-Orc ハーフオーク
::msgcat::mcset $loc Half-Troll ハーフトロル
::msgcat::mcset $loc Dunadan ドゥナダン
::msgcat::mcset $loc High-Elf ハイエルフ

# ANGBANDTK, KANGBANDTK, OANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc Kobold コボルド
::msgcat::mcset $loc Maia マイア
::msgcat::mcset $loc Ent エント
::msgcat::mcset $loc Wraith レイス
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc Maia マイア
::msgcat::mcset $loc Shadow*Fairy 影フェアリー
::msgcat::mcset $loc Ent エント

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc Human 人間
::msgcat::mcset $loc Half-Elf ハーフエルフ
::msgcat::mcset $loc Elf エルフ
::msgcat::mcset $loc Hobbit ホビット
::msgcat::mcset $loc Gnome ノーム
::msgcat::mcset $loc Dwarf ドワーフ
::msgcat::mcset $loc Half-Orc ハーフオーク
::msgcat::mcset $loc Half-Troll ハーフトロル
::msgcat::mcset $loc Amberite アンバライト
::msgcat::mcset $loc High-Elf ハイエルフ
::msgcat::mcset $loc Barbarian 野蛮人
::msgcat::mcset $loc Half-Ogre ハーフオーガ
::msgcat::mcset $loc Half-Giant ハーフジャイアント
::msgcat::mcset $loc Half-Titan ハーフタイタン
::msgcat::mcset $loc Cyclops サイクロプス
::msgcat::mcset $loc Yeek イーク
::msgcat::mcset $loc Klackon クラッコン
::msgcat::mcset $loc Kobold コボルド
::msgcat::mcset $loc Nibelung ニーベルング
::msgcat::mcset $loc Dark-Elf ダークエルフ
::msgcat::mcset $loc Draconian ドラコニアン
::msgcat::mcset $loc Mindflayer マインドフレア
::msgcat::mcset $loc Imp インプ
::msgcat::mcset $loc Golem ゴーレム
::msgcat::mcset $loc Skeleton スケルトン
::msgcat::mcset $loc Zombie ゾンビ
::msgcat::mcset $loc Vampire 吸血鬼
::msgcat::mcset $loc Spectre 幽霊
::msgcat::mcset $loc Sprite 妖精
::msgcat::mcset $loc Beastman 獣人

# ZANGBANDTK
}


if {[variant ANGBANDTK KANGBANDTK]} {

::msgcat::mcset $loc Warrior 戦士
::msgcat::mcset $loc Mage メイジ
::msgcat::mcset $loc Priest プリースト
::msgcat::mcset $loc Rogue 盗賊
::msgcat::mcset $loc Ranger レンジャー
::msgcat::mcset $loc Paladin パラディン

# ANGBANDTK, KANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc Illusionist 幻術師
::msgcat::mcset $loc Druid ドルイド
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc Warrior 戦士
::msgcat::mcset $loc Mage メイジ
::msgcat::mcset $loc Priest プリースト
::msgcat::mcset $loc Rogue 盗賊
::msgcat::mcset $loc Ranger レンジャー
::msgcat::mcset $loc Paladin パラディン
::msgcat::mcset $loc Druid ドルイド
::msgcat::mcset $loc Necromancer 死霊術師
::msgcat::mcset $loc Assassin アサシン

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc Warrior 戦士
::msgcat::mcset $loc Mage メイジ
::msgcat::mcset $loc Priest プリースト
::msgcat::mcset $loc Rogue 盗賊
::msgcat::mcset $loc Ranger レンジャー
::msgcat::mcset $loc Paladin パラディン
::msgcat::mcset $loc Warrior-Mage 魔法戦士
::msgcat::mcset $loc Chaos-Warrior 混沌の戦士
::msgcat::mcset $loc Monk モンク
::msgcat::mcset $loc Mindcrafter 超能力者
::msgcat::mcset $loc High-Mage ハイメイジ

# ZANGBANDTK
}

# Book names
if {[variant ANGBANDTK]} {
::msgcat::mcset $loc Magic
::msgcat::mcset $loc Prayer
}
if {[variant KANGBANDTK]} {
::msgcat::mcset $loc Magic
::msgcat::mcset $loc Prayer
::msgcat::mcset $loc Illusion
::msgcat::mcset $loc Nature
}
if {[variant OANGBANDTK]} {
::msgcat::mcset $loc Magic
::msgcat::mcset $loc Prayer
::msgcat::mcset $loc Druid
::msgcat::mcset $loc Necro
}
if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc Life 生命
::msgcat::mcset $loc Sorcery 仙術
::msgcat::mcset $loc Arcane 秘術
::msgcat::mcset $loc Trump トランプ
::msgcat::mcset $loc Nature 自然
::msgcat::mcset $loc Chaos カオス
::msgcat::mcset $loc Death 暗黒
}

# Stats
::msgcat::mcset $loc strength 腕力
::msgcat::mcset $loc intelligence 知力
::msgcat::mcset $loc wisdom 賢さ
::msgcat::mcset $loc dexterity 器用
::msgcat::mcset $loc constitution 耐久
::msgcat::mcset $loc charisma 魅力

