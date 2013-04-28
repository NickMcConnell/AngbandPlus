set loc ja

::msgcat::mcset $loc Weak 衰弱
::msgcat::mcset $loc Hungry 空腹
::msgcat::mcset $loc Full 満腹
::msgcat::mcset $loc Gorged 食過ぎ

::msgcat::mcset $loc Blind 盲目
::msgcat::mcset $loc Confused 混乱
::msgcat::mcset $loc Afraid 恐怖
::msgcat::mcset $loc Poisoned 毒
::msgcat::mcset $loc Paralyzed! 麻痺状態！

::msgcat::mcset $loc "Rest %d" "休息 %d"
::msgcat::mcset $loc "Rest *****"  "休息 *****"
::msgcat::mcset $loc "Rest &&&&&" "休息 &&&&&"
::msgcat::mcset $loc "Rep. %d" "回数 %d"
::msgcat::mcset $loc "Repeat %d" "回数 %d"
::msgcat::mcset $loc Searching 探索中

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc Normal 通常
::msgcat::mcset $loc Mouse 鼠
::msgcat::mcset $loc Ferret 鼬
::msgcat::mcset $loc Hound ハウンド
::msgcat::mcset $loc Gazelle ガゼル
::msgcat::mcset $loc Lion ライオン
::msgcat::mcset $loc Ent エント
::msgcat::mcset $loc Bat 蝙蝠
::msgcat::mcset $loc Werewolf 人狼
::msgcat::mcset $loc Vampire 吸血鬼
::msgcat::mcset $loc Wyrm
}

::msgcat::mcset $loc "Fast (%+d)" "加速 (%+d)"
::msgcat::mcset $loc "Slow (%+d)" "減速 (%+d)"

::msgcat::mcset $loc Study 学習

::msgcat::mcset $loc "Mortal Wound" 致命傷
::msgcat::mcset $loc "Deep gash" ひどい深手
::msgcat::mcset $loc "Severe cut" 重傷
::msgcat::mcset $loc "Nasty cut" 大変な傷
::msgcat::mcset $loc "Bad cut" ひどい傷
::msgcat::mcset $loc "Light cut" 軽傷
::msgcat::mcset $loc Graze かすり傷

::msgcat::mcset $loc "Knocked out" 意識不明瞭
::msgcat::mcset $loc "Heavy stun" ひどく朦朧
::msgcat::mcset $loc Stun 朦朧

::msgcat::mcset $loc Wizard ウィザード
::msgcat::mcset $loc Winner 勝利者

#
# Extra status messages
#
::msgcat::mcset $loc Blessed 祝福
::msgcat::mcset $loc Hero ヒーロー
::msgcat::mcset $loc Berserk バーサク
::msgcat::mcset $loc "Res Acid" 耐酸
::msgcat::mcset $loc "Res Cold" 耐冷
::msgcat::mcset $loc "Res Elec" 耐電
::msgcat::mcset $loc "Res Fire" 耐火
::msgcat::mcset $loc "Res Poison" 耐毒
::msgcat::mcset $loc "Prot Evil" 退悪
::msgcat::mcset $loc Shield シールド

if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
::msgcat::mcset $loc Invulnerable 無敵
}

::msgcat::mcset $loc Faster 加速
::msgcat::mcset $loc Slower 減速
::msgcat::mcset $loc Infravision 赤外線視力
::msgcat::mcset $loc "See Invis" 透明体視認
::msgcat::mcset $loc Recall 帰還
::msgcat::mcset $loc Hallucinate 幻覚

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc "T/S Anchor"
::msgcat::mcset $loc Ghostly
::msgcat::mcset $loc Invisible
::msgcat::mcset $loc "Res Cha/Con"
::msgcat::mcset $loc "Res Lig/Dar"
::msgcat::mcset $loc "Res Sou/Sha"
::msgcat::mcset $loc "Res Nether"
::msgcat::mcset $loc "Res Nexus"
::msgcat::mcset $loc "Levitate"
::msgcat::mcset $loc "Sus Str"
::msgcat::mcset $loc "Sus Int"
::msgcat::mcset $loc "Sus Wis"
::msgcat::mcset $loc "Sus Dex"
::msgcat::mcset $loc "Sus Con"
::msgcat::mcset $loc "Sus Chr"
}

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc "Att Acid"
::msgcat::mcset $loc "Att Elec"
::msgcat::mcset $loc "Att Fire"
::msgcat::mcset $loc "Att Cold"
::msgcat::mcset $loc "Att Poison"
::msgcat::mcset $loc ESP
::msgcat::mcset $loc "Res Magic"
::msgcat::mcset $loc Stealth
}

if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc ESP
::msgcat::mcset $loc Wraith 幽体
}

