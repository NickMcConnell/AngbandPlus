set loc ja

# Window title
::msgcat::mcset $loc "Character Creation" キャラクタ作成

# Font
::msgcat::mcset $loc font,prompt "{MS 明朝} 10 bold underline"

# Button label
::msgcat::mcset $loc "Options" オプション
::msgcat::mcset $loc "< Back" <戻る
::msgcat::mcset $loc "Next >" 進む>
::msgcat::mcset $loc "Quit" やめる

#Gender
::msgcat::mcset $loc "Choose your character's gender" "《性別》を選んでください"

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc D_Gender \
	"Females start with more money, and weigh\
	less, but otherwise your character's gender does not have any\
	significant gameplay effects."
} else {
::msgcat::mcset $loc D_Gender \
	"《性別》の違いはゲーム上特に影響を及ぼしません。"
}
::msgcat::mcset $loc Female 女性
::msgcat::mcset $loc Male 男性

#Race
::msgcat::mcset $loc "Choose your character's race" "種族を選んで下さい"
::msgcat::mcset $loc D_Race \
	"《職業》によってキャラクターの先天的な能力やボーナスが変化します。"

# HTML
::msgcat::mcset $loc "Stat Modifiers"
::msgcat::mcset $loc Skills
::msgcat::mcset $loc Attributes
::msgcat::mcset $loc "Recommended Classes"
::msgcat::mcset $loc attribute "%s (level %d)"

#Class
::msgcat::mcset $loc "Choose your character's class" "職業を選んでください"
::msgcat::mcset $loc D_Class \
	"それぞれの職業には特殊能力と弱点があります"

#Plot
::msgcat::mcset $loc "Choose a plot" "プロットを選んでください"
::msgcat::mcset $loc D_Plot \
	"プロットにより達成すべきクエストが変わります。"

#Realm
::msgcat::mcset $loc "Choose your character's first realm of magic" \
	"第一の魔法の領域を選んでください"
::msgcat::mcset $loc "Choose your character's second realm of magic" \
	"第二の魔法の領域を選んでください"
::msgcat::mcset $loc D_Realm \
	"領域により習得できる魔法の種類が異なります。"


#Race Description

if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
::msgcat::mcset $loc D_Human [join {
人間は基本となる種族です。全ての他の種族は人間との比較によっ
てその特徴が述べられます。人間は全ての職業を選択することがで
き、全てにおいて平均的な能力を発揮します。人間は他の種族より
もレベルの上がり方が早いのですが、それは寿命が短いからです。
人間は種族固有の修正値や資質を持っていません。
} ""]

::msgcat::mcset $loc D_Half-Elf [join {
ハーフエルフは人間より知能が高く、素早いですが、腕力は劣る種
族です。ハーフエルフが多少優れている能力は、探索、解除、魔法
防御、隠密行動、遠距離攻撃、魔法で、劣っている能力は直接攻撃
です。ハーフエルフはどの職業にもつけます。また種族固有の資質
を持っていません。
} ""]

::msgcat::mcset $loc D_Elf [join {
エルフは人間より魔法を使用する能力に長けていますが、戦闘能力
は劣ります。エルフは人間やハーフエルフより知能が高く動きが素
早いという特徴を持ち、また賢さも優れています。エルフが秀でて
いる能力は、探索、解除、知覚、隠密行動、遠距離攻撃、魔法で、
劣っている能力は直接攻撃です。エルフはパラディンを除く全ての
職業に就くことができます。種族固有の資質として閃光への耐性を
持っています。
} ""]

::msgcat::mcset $loc D_Hobbit [join {
ホビット(ハーフリング)は遠距離攻撃が大変得意で、加えて高い魔
法防御力も持っています。他に大変優れている能力は、探索、解除、
知覚、隠密行動です。つまりホビットはひじょうに優秀な盗賊にな
ることができますが、盗人と呼ばれることは好みません。ホビット
は人間よりずっと力が弱く、直接攻撃が大変苦手です。しかし高い
赤外線視力を持っていて、体温を持った生物を遠くから発見するこ
とができます。ホビットが選択できる職業は戦士、メイジ、盗賊で
す。種族固有の資質として器用さを維持することができます。
} ""]

::msgcat::mcset $loc D_Gnome [join {
ノームはドワーフより体が小さいのですが、ホビットよりは大きい
種族です。ノームはホビットと同様に大地に洞穴の様な家を作って
暮らしています。ノームはこの上なく優秀なメイジになることがで
き、またとても優れた魔法防御力を持っています。そして探索、解
除、知覚、隠密行動に優れた能力を発揮します。ノームは人間より
力が弱いので武器を使った攻撃は苦手です。ノームは優れた赤外線
視力を持っていますので、温血動物を遠くから認識することができ
ます。ノームの選択できる職業は戦士、メイジ、プリースト、盗賊
です。ノームの種族固有の資質は麻痺と減速への耐性です。
} ""]

::msgcat::mcset $loc D_Dwarf [join {
ドワーフは頑固な鉱夫で伝説的な戦士です。ダンジョンは彼らにとっ
ては我が家みたいなものですので、戦士やプリーストという職業は
最高の選択になります。ドワーフは人間と比べて力が強くタフです
が、動きは遅くて知能が低いのが欠点です。彼らはひじょうに頑固
で幾分賢いので、投げかけられた呪文にかかりにくい傾向がありま
す。またドワーフの住みかは地下ですので、ひじょうに優れた赤外
線視力を持っています。しかしドワーフには致命的な欠点が一つあ
ります。それは、ドワーフは騒々しくて虚栄心が強く、大声で歌を
歌ったり、意味もなく大声で独り言を言ったり、自分で空想した敵
に叫び声を上げたりする、ということです。つまり、ドワーフの隠
密行動力は絶望的という事です。ドワーフは固有の資質として決し
て盲目になりません。
} ""]

::msgcat::mcset $loc D_Half-Orc [join {
ハーフオークは素晴らしい戦士や立派なプリーストになることがで
きますが、魔法は全然ダメです。彼らはドワーフと同様に隠密行動
能力が低く、探索や解除や知覚能力はまったく見込みがありません。
一目見れば分かりますが、ハーフオークの外見ははっきり言って醜
悪です。ですから町でアイテムを買うには余計なお金を払わなけれ
ばなりません。ハーフオークはとても素晴らしい耐久力と多くのヒッ
トポイントを持っているという理由から、よい戦士や盗賊になるこ
とができます。ハーフオークは地上に住むよりも地下に住む方を好
むという性質から、暗闇の攻撃への耐性を持っています。
} ""]

::msgcat::mcset $loc D_Half-Troll [join {
ハーフトロルは信じられないくらい力が強く、他のどんな種族より
も多いヒットポイントを持っています。そのかわり彼らは非常に愚
かで動きが鈍いのです。ハーフトロルは偉大な戦士か怪しげなプリー
ストになることができます。探索、解除、知覚、隠密行動は彼らの
苦手とするところです。ハーフトロルはひじょうに醜く、その醜さ
にはハーフオークでさえ顔をしかめるほどです。またハーフトロル
は何故か突如走り出して喜んだりします。ハーフトロルは常に腕力
を維持します。
} ""]

::msgcat::mcset $loc D_Dunadan [join {
ドゥナダンは西方から来た屈強な種族です。このいにしえの種族は
全ての領域において人間の能力を凌駕し、特に耐久力に関してはそ
れが顕著です。しかしながらこの種族は全てに卓越していることが
災いして、この世界には新しい経験といったものがほとんどなく、
レベルを上げることが非常に困難です。ドゥナダンは全ての職業を
選択することができます。ドゥナダンの耐久力は決して減少しませ
ん。
} ""]

::msgcat::mcset $loc D_High-Elf [join {
ハイエルフはこの世の初めから存在する不死の種族です。レベルを
上げることはドゥナダンよりも大変なことです。彼らは全ての能力
が卓越していて、力が強くて知能が高いのですが、賢さは少し疑わ
しいものがあります。彼らはパラディン以外の全ての職業を選択す
ることができ、どの職業に就いても素晴らしい能力を発揮します。
ハイエルフは生まれたときから透明なものを見ることができ、更に
普通のエルフと同様に閃光への耐性も持っています。
} ""]

# ANGBANDTK, KANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc D_Kobold ""
::msgcat::mcset $loc D_Maia ""
::msgcat::mcset $loc D_Ent ""
::msgcat::mcset $loc D_Wraith ""
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc D_Maia\
"Special race for players new to Oangband.  Score is reduced by 50%.\
The Maiar, lesser deities of the created world, often\
take tangible form and mingle among earthly beings.  They can be found\
among the ranks of wise counselors, peerless warriors, mighty wizards and\
necromancers; indeed they can play any class with credit.  Their vital\
statistics put all other races to shame, their standard abilities are\
always good and sometimes superlative, and they recover divinely\
quickly from all sorts of wounds, poison, stunning, confusion, blindness,\
and paralyzation.  (They do not, however, get any innate resistances\
or special abilities).  Despite all these qualities, Maiar advance fairly\
quickly, making them the perfect race for those desiring a character able\
to survive the perils of an unfamiliar game."

::msgcat::mcset $loc D_Shadow*Fairy\
"Also known as Fays, these beings were created\
neither by the powers of good nor of evil but, for all anyone knows,\
appeared when the world was formed out of primeval darkness and void.  They\
are physically weak and perilously frail, but possess considerable powers\
of intellect and understanding.  They know little of combat, and only have\
modest advantages in disarming and magical device skill - but all this is\
offset by incomparable concealment.  Not merely can Shadow Fairies\
wreath themselves in murk and gloomy night, thus gaining by far the\
highest stealth of any race, but they never aggravate monsters.  (If\
they normally would, they suffer a penalty of three to stealth).\
Shadow Fairies must beware of bright light."

::msgcat::mcset $loc D_Ent\
"Clad in bark-like skin, and with bodies like the trunks\
of trees, Ents can nevertheless move about perfectly well.  They have poor\
dungeon survival skills, and are bad at archery and with polearms, but can\
fight better than average with other weapons.  As long as they stay\
away from fire (fire damage is increased by 33%.  Resistances operate\
normally after this.), Ents will find themselves well capable of surviving\
many a bitter fight.  As they age, Ents slowly lose the relative\
suppleness of their sapling youth, trading dexterity for increased\
strength and constitution.  Members of this wise race can never gain\
feather fall by any means, but will slowly acquire the ability needed\
to tear rock apart with their weaponless hands."

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc D_Human [join {
人間は基本となるキャラクタです。他の全ての種族は人間と比較
されます。人間はどんなクラスに就くこともでき、どのクラスでも
平均的にこなせます。人間は寿命が短いため、レベル上昇が他のど
んな種族よりも早くなる傾向があります。また、特別な修正や特性
は持っていません。
} ""]

::msgcat::mcset $loc D_Half-Elf [join {
ハーフエルフは人間より賢いですが、強くはありません。彼らは探
索能力、解除能力、魔法回避能力、隠密行動能力、射撃能力、そし
て魔法でわずかに優れています。しかし武器の取り扱いはそう得意
ではありません。ハーフエルフはどのクラスに就くこともでき、生
まれつきの特性はありません。
} ""]

::msgcat::mcset $loc D_Elf [join {
エルフは人間より良い魔法使いになれますが、戦闘は苦手です。彼
らは人間やハーフエルフよりも賢く、高い賢さを持っています。エ
ルフは探索能力、解除能力、知覚能力、隠密行動能力、射撃能力、
そして魔法で優れていますが、武器の扱いは得意ではありません。
彼らは生まれつき光に対する耐性を持っています。
} ""]

::msgcat::mcset $loc D_Hobbit [join {
ホビット、またはHalflingは射撃能力や投擲能力に長け、
魔法回避能力も優れています。また、探索能力、解除能力、知覚能
力、そして隠密行動能力でもとても良い能力を示します。そのため
、彼らは優れた盗賊となることができます（しかし、しのびのもの
と呼ばれることを好みます）。ホビットは人間より遥かに貧弱で、
戦士としてはてんでダメです。彼らはかなり良い赤外線視力を持っ
ており、温血動物を離れた場所から見つけることができます。ホビ
ットは器用を下げられることがありません。彼らは食事がとても好
きで、あるレベルになると手に入る食材から「ごちそう」を作るこ
とが出来るようになります。
} ""]

::msgcat::mcset $loc D_Gnome [join {
ノームはドワーフより小さいですが、Halflingよりは大き
い種族です。彼らはホビット同様地表の洞穴のような家に住んでい
ます。ノームはとても良い魔法回避能力を持ち、探索能力、解除能
力、知覚能力、隠密行動能力でも優れています。彼らは人間より低
い腕力を持ち、武器を持っての戦闘は苦手です。ノームはかなり良
い赤外線視力を持っており、温血動物を離れた場所から見つけるこ
とができます。ノームは生まれつき麻痺に対する耐性を持っていま
す。レベルが上がると自由にテレポートできるようになります。
} ""]

::msgcat::mcset $loc D_Dwarf [join {
ドワーフは頑固な坑夫であり、伝説の戦士です。彼らは人間にくら
べ強くタフですが、知能やスピードは劣ります。彼らはとてもわが
ままでいくらか賢いので、魔法への耐性があります。彼らは地下に
住むので良い赤外線視力を持ちます。しかし彼らは一つ大きな欠点
を持っています。声が大きくてプライドが高く、大声で歌って意味
もなく議論し、いもしない敵にむかっていつも叫びまくっています
。言い替えればドワーフの隠密行動能力は絶望的に悪いです。彼ら
は決して盲目にはなりません。ドワーフはまたダンジョンの構造を
学ぶようになり、他の種族に見えない物を見えるようになります。
} ""]

::msgcat::mcset $loc D_Half-Orc [join {
ハーフオークはよい戦士になれますが、魔法は期待できません。彼
らはドワーフと同じくらい隠密行動能力が悪く、また探索能力や解
除能力、知覚能力もひどいです。ハーフオークは醜く、店での買い
物ではより高い金額を要求されがちです。ハーフオークは高い耐久
やHPを持つという単純な理由から良い戦士や盗賊になります。彼
らは地下に住むことを好むため、ハーフオークは暗闇に対する耐性
を備えています。ハーフオークは恐怖を打ち払う能力を得るように
なるでしょう。
} ""]

::msgcat::mcset $loc D_Half-Troll [join {
ハーフトロルは信じられないほど強く、他の大部分の種族より大き
なHPを持ちます。彼らは不運にもとても愚鈍ですが、良い戦士や
ほどほどのプリーストになれます。彼らの探索能力、解除能力、知
覚能力、隠密行動能力は悪く、その外見はハーフオークがしかめっ
面をするほど醜悪です。ハーフトロルは腕力が下がることがありま
せん。レベルが上がると、彼らは再生能力を手にいれ、凶暴化する
能力も得ます。
} ""]

::msgcat::mcset $loc D_Amberite [join {
アンバライトは多くのアドバンテージを授けられた、うわさによれ
ば不死の種族です。彼らは知覚能力、打撃能力、射撃能力に優れて
おり、他の面でもかなり熟練しています。彼らはとてもタフで頑強
であり、彼らの耐久が下がることはありません。また、怪我を治す
再生能力は他の種族を圧倒します。事実上あらゆるものを見てきて
おり、新鮮なものはほとんどないため、彼らの成長は他のどの種族
より遅いものです。しかし充分成長すれば、彼らは固有の能力であ
るパターンウォークとシャドウシフトの能力を得ます。
} ""]

::msgcat::mcset $loc D_High-Elf [join {
上のエルフは世界の始まりから存在する不死の種族です。彼らは全
てのスキルに熟達しており、強く、知的ですが賢さは高いとは限り
ません。彼らはほとんどの職業をうまくこなすでしょう。上のエル
フは見えないものを見ることができ、普通のエルフ同様光に対する
耐性を持っています。しかし、彼らにとって未知のものはほとんど
なく、経験を得ることは大変に困難です。
} ""]

::msgcat::mcset $loc D_Barbarian [join {
野蛮人は北方から来た頑強な種族です。彼らは激しく戦い、彼らの
激怒は世界中で恐れられています。戦闘が彼らの人生です。彼らは
恐れを知らず、ハーフトロルよりもすぐに狂暴に戦闘に入ってしま
うことを学びます。しかし、野蛮人は魔法を疑っており、そのため
魔法の道具を使うことはかなり大変なこととなっています。また魔
法を使う職業はかなり困難です。
} ""]

::msgcat::mcset $loc D_Half-Ogre [join {
ハーフオーガはハーフオークに似ていますが、それだけではありま
せん。彼らは大きく、邪悪で愚かです。戦士としては彼らは必要な
資質を全て持っており、また魔法使いになることさえできます。結
局、彼らはオーガ=メイジに関係があり、レベルが十分に上がった
ら彼らから罠のルーンをセットするスキルを学ぶのです。ハーフオ
ークのように、彼らは暗闇に対する耐性を持ち、ハーフトロル同様
に腕力が下がることはありません。
} ""]

::msgcat::mcset $loc D_Half-Giant [join {
半巨人はそんなに変わってはいません。なぜなら巨人（特に巨人娘
）と結婚することは高貴なことであるという伝統があったからです
。にもかかわらず、そのようなカップルの不幸な子孫は人間世界で
普通に見られることがほとんどありません。半巨人は大変力強いの
ですが、呪文を唱えられるほど利口ではありません。彼らはよい戦
闘能力を持ちますが、それ以外のことは苦手です。普通の壁で巨人
や半巨人の怒りに耐えられるものはなく、レベルが上がると穴を掘
る能力を得ます。彼らの厚い皮膚は破片に対する耐性を持ちます。
また、ハーフオーガやハーフトロル同様腕力を下げられることがあ
りません。
} ""]

::msgcat::mcset $loc D_Half-Titan [join {
巨大なTitanと人間の半分不死の子孫であり、この強大な生物
は他のほぼ全ての種族よりはるかに勝っています。彼らは他種族に
みられるような魅力的な特殊能力は持っていませんが、強力な属性
はそれを補ってあまりあります。彼らは敵の強さを見抜くことがで
きるようになります。法と秩序を愛する彼らは、混沌に対する耐性
を持っています。
} ""]

::msgcat::mcset $loc D_Cyclops [join {
一つ目ではありますが、サイクロプスは多くの二つの目を持つ生物
以上に見ることができます。サイクロプスはわがままで、うるさい
音は彼らを悩ますことはありません。彼らは魔法を使う職業には向
いているとは言えません。しかしあるユリシーズという人が示した
ように、彼らの岩を投げる正確さはおそろしいものです。
} ""]

::msgcat::mcset $loc D_Yeek [join {
イークは最も哀れな生物の一つです。幸運なことに彼らの恐ろしい
叫び声はあまり勇気をもってない敵を怖がらせ、その皮膚はレベル
が上がるとだんだん酸に対する耐性を得てきます。とはいえ、ごく
普通のモンスターでさえ "wipe the proverbial"
" floor with an unwary Yeek."
} ""]

::msgcat::mcset $loc D_Klackon [join {
クラッコンは奇怪な半知的の昆虫型生物です。彼らはすばらしい戦
士になれますが、精神的な能力はひどく制限されています。彼らの
集合精神は混乱することがありません。また彼らは敏捷で、レベル
が上がるごとにスピードが速くなります。また体内に酸を持ち、酸
に対する耐性があり、かつレベルが上がると酸を吐くことができま
す。
} ""]

::msgcat::mcset $loc D_Kobold [join {
コボルドは弱いゴブリンの種族です。彼らは毒を持った武器を好み
、毒矢（無制限に供給されます）を投げる能力を身につけることが
できます。コボルドはそこそこの戦士になれますが、その他のスキ
ルは軒並み悪いです。彼らは生まれつき毒に対する耐性を持ってい
ます。
} ""]

::msgcat::mcset $loc D_Nibelung [join {
嫌われ、迫害されてきた夜の小人族です。洞穴居住者である彼らは
、暗闇に悩まされることはありませんし、生まれつき持っている魔
法のアイテムに対する嗜好のため、彼らは装備による魔法のボーナ
スを奪う効果に耐性を持っています。また普通のドワーフのように
ダンジョンを調べ、罠や隠し扉を発見することができます。
} ""]

::msgcat::mcset $loc D_Dark-Elf [join {
闇の、洞穴に住む種族であるダークエルフは暗闇の攻撃に悩まされ
ず、また魔法の知識に対する長い伝統を持っています。その知能の
高さから彼らは素晴らしいメイジやプリーストになれます。また低
いレベルでもマジックミサイルを放つことができます。また鋭い視
力によって上のエルフ同様見えないものをみる能力を学びますがそ
れは上のエルフより高いレベルを必要とします。
} ""]

::msgcat::mcset $loc D_Draconian [join {
ドラゴンのような特性を持った人間型種族です。彼らはレベルが上
がるにつれ、新しいelementへの耐性を手にいれます（最後
には毒に対する耐性を手にいれます）。またレベルとともに強力に
なるブレスを吐くことができます。ブレスの種類は職業とレベルに
よります。その翼で、彼らは簡単に落とし穴や溶岩、水を無傷で飛
び越えることができます。
} ""]

::msgcat::mcset $loc D_Mindflayer [join {
秘密主義の神秘的な古代種族です。彼らの文明はこの惑星上の何よ
りも古いかもしれません。その肉体的資質は決して誉められたもの
ではありませんが、彼らの知能と賢さはマインドフレアを他のどん
な種族よりも強力な魔法使いにします。マインドフレアの知能と賢
さは下がることがなく、レベルが上がると見えないものをみる能力
、テレパシー、精神攻撃の能力を獲得します。
} ""]

::msgcat::mcset $loc D_Imp [join {
地獄からやってきた悪魔的な生物です。インプは生まれつき火に耐
性を持っており、ファイアボルトとファイアボールを放つ能力を得
ることができます。彼らは他の種族から毛嫌いされていますが、大
抵の職業をかなりうまくこなすことができます。
} ""]

::msgcat::mcset $loc D_Golem [join {
ゴーレムは泥のような生命のない材料からつくられ、生命を吹き込
まれたた人工的な生物です。彼らには思考というものがほとんどな
く、そのため魔法に依存する職業では役立たずです。しかし戦士と
しては大変にタフです。彼らは毒に耐性を持ち、見えないものを見
ることができ、さらに麻痺知らずです。高いレベルで、彼らは生命
力吸収攻撃に耐性を持つようになり、また一時的に肌を石のように
固くすることができるようになります。ゴーレムは通常の食物から
はほとんど栄養を摂取できないので空腹充足の巻物を集める必要が
あります。さもなくば体を動かしている魔法の力が切れて消滅して
しまいます。また、その頑丈な身体のため、ACにボーナスを得る
ことができます。
} ""]

::msgcat::mcset $loc D_Skeleton [join {
骸骨には２つのタイプが存在します。普通に見られる戦士タイプの
骸骨と、リッチと呼ばれる呪文を使う骸骨です。アンデッドである
彼らは、毒や生命力吸収攻撃を心配する必要はほとんどありません
。彼らは物体を知覚するのに眼を利用していないために、見えない
ものに騙されることはありません。彼らの骨はとがった破片のよう
なものに耐性を持ち（切られるものがないので）、早い段階で冷気
に対する耐性を獲得します。万一その残り少ない生命力を奪われる
ような不幸な目にあった場合、彼らはそれを自分の意思で取り戻す
ことができます。薬や食物の持つ効果は骸骨の胃（存在しませんが
）を通過することなくその力を発揮しますが、薬や食物自体は彼の
顎を通り抜けて落ちてしまい、栄養を吸収することはできません。
} ""]

::msgcat::mcset $loc D_Zombie [join {
骸骨同様、ゾンビもアンデッドです。彼らは生命力吸収攻撃に耐性
を持ち、生命力を取り戻す能力を得ることができます。また骸骨の
ように(彼らより早く)冷気への耐性を獲得します。また、毒に耐
性を持ち見えないものを見ることができます。（骸骨とは違い）切
る攻撃には弱いですが、地獄に対する耐性を持っています。ゴーレ
ムのように、彼らは食物からほとんど栄養を接種できません。とは
いえ、ゾンビはその名が示す通り心を持ちません。彼らの中ではグ
ルーは天才に見えるでしょう。
} ""]

::msgcat::mcset $loc D_Vampire [join {
強力なアンデッドの一種である吸血鬼は、畏敬の念を呼び起こす外
見をしています。しかしこの強力な生物は深刻な弱点を持っていま
す。太陽光線（や光源）は彼らの破滅を意味し、彼らは完全に日没
するまで地中深くに身を潜めていなくてはならないのです。一方、
暗闇は彼らをより強力にし傷つけることはありません。アンデッド
の例にもれず、彼らも生命力を吸収されることがなく、地獄に対す
る耐性を持っています。また、冷気と毒に対する耐性も備えていま
す。しかし、新鮮な血液に常に飢えており、それは近くにいる生物
から血液を吸うことによってのみ満たされます。これは吸血鬼の特
殊能力です。気を付けねばならないのは、彼らは非常に光に弱いの
で、日光で満たされたアーティファクトを装備したらダメージを受
けるということです。幸運にも、彼らはそのようなアイテムを必要
としません。その身体から「暗い光」のオーラを放出しているから
です。また光に対する耐性があれば、どんな時でも日光からの悪影
響を受けることはありません。
} ""]

::msgcat::mcset $loc D_Spectre [join {
もう一つの強力なアンデッドが幽霊です。彼らはぞっとするような
幽霊で、不気味な緑色の光に包まれています。彼らは我々の存在す
る物質次元には部分的にしか存在していません。半物質的な存在で
ある彼らは、壁を通り抜けることができますが、そのときには壁の
密度によって傷つけられてしまいます。幽霊は恐ろしい叫び声をあ
げて弱いモンスターを恐怖させることができます。他のアンデッド
同様、彼らも生命力を吸収されることがなく、見えないものを見る
ことができ、毒と冷気に対して耐性を備え、さらに地獄に対する耐
性も持っています。実際のところ、彼らの半物質的な形態は地獄の
効果によってさらに強化されるのです。レベルが上がると彼らはテ
レパシーを獲得します。幽霊は卓越した魔法使いになることができ
ますが、その身体的特性は非常に貧弱です。ゴーレムやゾンビ同様
、彼らも通常の食物から栄養をほとんど得られません。
} ""]

::msgcat::mcset $loc D_Sprite [join {
妖精は非常に小さいです。彼らは小さな翼を持ち、罠や危険な地形
を飛び越えることができます。彼らは日光を大変好み、光に対する
耐性を持っています。身体的にはもっとも貧弱な種族の一つですが
、妖精は魔法の面で大変な才能を持っており、非常に熟練した魔法
使いになることができます。妖精は眠り粉を撒く特殊能力を持ち、
高レベルではより速く飛ぶことができるようになります。
} ""]

::msgcat::mcset $loc D_Beastman [join {
この種族はカオスによってつくられた冒涜的で嫌悪される存在です
。彼らは独立した種族ではなく、人間型種族、大抵は人間がカオス
によって歪められた存在、もしくは人間と獣の悪夢のような交配種
です。全ての獣人はカオスに盲従しており、そのため混乱と音に対
して耐性を備えていますが、純粋なログルスはまだ彼らに対し効果
を持っています。獣人は混沌を好み、それは彼らをさらに歪めます
。獣人は突然変異を起こしやすい性質を持っています。彼らがつく
られたとき、ランダムな変異を受けます。その後、レベルが上がる
ごとに違う変異を受ける小さな可能性があります。
} ""]

# ZANGBANDTK
}

#
# Descriptions of classes
#

if {[variant ANGBANDTK KANGBANDTK]} {

::msgcat::mcset $loc D_Warrior [join {
戦士は切って切って切りまくるキャラクターで、困難にぶつかると
とりあえず相手を叩き切ることで答えを見つけ出そうとします。し
かしやはり時には魔法のアイテムの力を借りることもあるでしょう。
戦士にとって大切なステータスは腕力と耐久力で、更に器用さも役
に立つことが多いでしょう。戦士は直接攻撃能力と遠距離攻撃能力
が優れていますが、それ以外は劣っています。
} ""]

::msgcat::mcset $loc D_Mage [join {
メイジは機知をもって生き抜いていかなければなりません。腕力を
もってダンジョンを切り開いていくことをメイジに望んではいけま
せん。それゆえメイジはその魔法によって敵を討ちまかし、欺き、
混乱させ、逃げのびなければなりません。メイジは自分の呪文に加
えて魔法のアイテムを上手に活用しなければ目的を達成することは
できないでしょう。メイジはどんなキャラクターよりも早くに高レ
ベルの魔法のアイテムを使用できるようになります。また投げかけ
られた呪文に対する魔法防御力は最も高いのです。知能と器用さは
メイジにとって最も大切なステータスです。メイジが屈強の戦士に
なってはいけないという法はありませんが、メイジの最も得意とす
る分野はやはり魔法なのです。
} ""]

::msgcat::mcset $loc D_Priest [join {
プリーストは神聖な敬虔さを持ったキャラクターです。プリースト
がダンジョンに入るのは、その中にはびこる邪悪を滅ぼすという目
的があるからです。もし財宝がたまたまザックの中に転がり込んで
きても、そのほとんどを教会の栄光のために寄付してしまいます。
プリーストはその祈りを神から授かりますので、学びたい祈りを自
分で選択することはできません。プリーストは、魔法のアイテムを
「神から授かりしアイテム」と呼ぶことを好み、その使用には長け
ていますが、メイジほど上手に使用できるわけではありません。プ
リーストは魔法防御力が高く、かなりの戦士になる素質も持ってい
ます。しかしその武器の選択に際しては、刃の付いた武器より鈍器
を好みます。賢さと魅力はプリーストにとって最も大切なステータ
スです。
} ""]

::msgcat::mcset $loc D_Rogue [join {
盗賊はその狡猾さによって生きぬいていこうとするキャラクターで
すが、いざというときには戦闘によって道を切り開くこともできま
す。盗賊はトラップや鍵に関してはお手の物で、どんな仕掛けも彼
にとってはおもちゃのようなものです。盗賊は高い隠密行動能力を
持っていて、多くの生物と戦うことなしにその側を歩き回ることが
できて、こっそり忍び寄って先手を取ることも可能です。盗賊の知
覚能力はどのキャラクターよりも優れたもので、探索をしなくても
多くのトラップやドアに気付くことがあるでしょう。盗賊は戦士や
パラディンよりは上手に魔法のアイテムを使いますが、それほど頼
りにできるものではありません。盗賊は多少の呪文を学ぶことがで
きますが、メイジが使うような強力な攻撃呪文は無理というもので
す。盗賊にとって最も大切なステータスは知能と器用さです。
} ""]

::msgcat::mcset $loc D_Ranger [join {
レンジャーは戦士とメイジの両方を兼ね備えた職業です。レンジャー
は屈強な戦士であり、弓のような遠距離攻撃の武器を最も上手に扱
える職業です。レンジャーはメイジよりゆっくりと呪文を学んでい
きますが、最強の呪文を除いた全ての呪文を学ぶことができます。
レンジャーは現実的に二つの職業を兼ねていますので、レベルを上
げるためにはより多い経験値を必要とします。レンジャーは、隠密
行動能力、知覚能力、探索能力、魔法防御力、魔法道具使用能力、
が優れています。最も大切なステータスは知能と器用さです。
} ""]

::msgcat::mcset $loc D_Paladin [join {
パラディンは戦士とプリーストの両方を兼ね備えた職業です。パラ
ディンの戦闘能力は戦士に次いで素晴らしいものですが、遠距離攻
撃用武器を使用するのは苦手です。パラディンはプリーストよりゆっ
くりと祈りを獲得していきますが、最終的には最強の祈りを含めて
全ての祈りを獲得することができます。パラディンは現実的に二つ
の職業を兼ねていますので、レベルを上げるためにはより多い経験
値を必要とします。能力に関してはパラディンは多くが劣っていま
す。隠密行動能力、知覚能力、探索能力、魔法道具使用能力が劣っ
ています。パラディンにとって最も大切なステータスは腕力と魅力
です。
} ""]

# ANGBANDTK, KANGBANDTK
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc D_Illusionist ""
::msgcat::mcset $loc D_Druid ""
}

if {[variant OANGBANDTK]} {

::msgcat::mcset $loc D_Warrior\
"The great melee fighter, capable of doing more damage and surviving\
longer at close quarters than any other class.  So skilled in combat\
can a warrior become that he gains permanent protection from fear at\
level 30, can spread attacks out among weaker monsters at level 35, and\
regenerates intrinsically at level 45.  He is unchallengable with melee\
weapons, and is also good with all missile launchers, getting an extra\
shot with bows (level 35) and crossbows (level 40).  His biggest\
drawback is his ignorance of spells and many magical items.  Most of\
his dungeon survival skills are mediocre.  A Warrior learns no magical\
spells, and has poor magical defences.  He has very fast, strong\
pseudo-ID.  His prime stat is Strength, and a good Dexterity and\
Constitution can really help at times."

::msgcat::mcset $loc D_Mage\
"Master of sorcery.  A mage is not really complete without an assortment\
of magical devices to use in addition to his spells.  He can master\
the higher level magical devices far more easily than anyone else.\
He fights very poorly, except with the lightest of weapons.  He is bad\
at archery.  A mage is stealthy at low levels, but his magic betrays\
his presence at high.  Most of his other skills are mediocre.  He has\
excellent magical defences, but dangerously poor physical ones.  He has\
very slow, weak pseudo-ID.  Intelligence is his primary stat, and a\
high Constitution is very useful."

::msgcat::mcset $loc D_Priest\
"Covenented with a mighty and generous god. He explores the dungeon only\
to destroy the evil that lurks within, and if treasure just happens to\
fall into his pack, well, so much more to the glory of his church!\
A priest is spoilt for choice with protection and healing prayers, and\
posesses an unequalled collection of knowledge gaining prayers.  He is\
a fair fighter with blunt weapons and miserable with edged.  He is poor\
at archery.  His magical defences are excellent, and his physical ones\
acceptable.  Priests are familiar with magic items.  He has merely\
mediocre stealth and dungeon survival skills.  Priests gain +1 to light\
radius at level 35 and above and permanant protection against evil at\
level 50.  He has very fast but weak pseudo-ID.  Wisdom is a Priest's\
primary stat."

::msgcat::mcset $loc D_Rogue\
"The flexible dungeon sneaker.  First in line when dungeon survival\
skills were handed out.  He can fight very well with light weapons and\
is dangerous with a sling (gets extra shots at 20th and 40th level).\
He is clumsy with heavy weapons.  He has decent magical skills.  A\
Rogue will have a fairly decent collection of spells to improve his\
natural skills, aid in battle, and gain information, but almost no\
ability to deal magical damage.  He can fight hand-to-hand more\
quietly than can any other class, and gets the most effective sneak\
attacks against sleeping monsters.  Rogues have the unique capacity of\
filching purses and setting traps (use the '+' command).  See the help\
file on combat for more details.  Unfortunately, no shopkeeper trusts\
a rogue, and so members of this class will pay dearly for goods in\
town.  A Rogue has acceptable physical and magical defences.  He has\
moderately fast, strong pseudo-ID.  His primary stats are Intelligence\
and Dexterity."

::msgcat::mcset $loc D_Ranger\
"An archer skilled in nature lore.  He is better than any other class\
with any missile weapon except slings.  A Ranger gets one extra shot\
with any missile weapon (25th level for slings, 45th for xbows), and\
two with bows (20th and 40th level).  He is a merely acceptable melee\
fighter, yet not as limited to light weapons as is a Rogue.  A ranger\
can use magical items effectively, and finds his spells handy but not\
critical.  A Ranger has decent dungeon survival skills, and good\
stealth.  He has acceptable magical and physical defences.  He has\
fairly slow, weak pseudo-ID.  His primary stats are Wisdom and\
Dexterity."

::msgcat::mcset $loc D_Paladin\
"A champion of the Faith.  A paladin is the second best melee fighter\
available, as long as he sticks to blunt weapons.  Missile weapons\
confuse him.  He has little stealth, and poor dungeon survial skills, but\
can survive as well or better than any other class in battle or against\
magic.  He has a sub-set of the priestly prayers.  Plus one to light\
radius at level 45 and above.  He has moderately-paced, strong\
pseudo-ID.  His primary stats are Wisdom and Strength."

::msgcat::mcset $loc D_Druid\
"At one with nature.  He posesses a wide and varied range of healing, \
dungeon alteration, and knowledge spells, plus a large number of spells \
to hinder and reduce foes.  Can fight well bare-handed and acceptably \
with slings, but not with other missile weapons or any melee weapon.  \
Although a druid can do a lot of damage, his spells are more random \
than are those of other realms.  He has decent magical skill, fairly \
high stealth, and good magical defences.  A Druids will have decent \
dungeon survival skills.  He has extremely slow, weak pseudo-ID.  His \
primary stat is Wisdom."

::msgcat::mcset $loc D_Necromancer\
"Dealer in death.  He has an unsurpassed collection of highly deadly\
combat spells, and can dish out damage like nobody's business.  His\
magical item ability is acceptable, but virtually all of his other\
skills are poor, and he has a hard time protecting himself.  Fortuately,\
at higher levels his magic can help with some of these problems.  He\
has slow, weak pseudo-ID.  His primary stat is Intelligence,\
and he needs to watch his Constitution."

::msgcat::mcset $loc D_Assassin\
"The death-dealing stalker.  He is better than a Rogue at melee, but is\
still limited to reasonably light weapons.  An Assassin is deadly with\
lighter missile launchers, and can eventually poison missiles, but gets\
no innate extra shots.  He is a master at using thrown weapons against\
sleeping monsters.  He has rather good stealth, but lacks protection\
if caught.  His other dungeon survival skills are good, and he has\
access to a sub-set of the necromantic spells.  He can shoot missiles\
and throw objects more quietly than any other class, and makes little\
noise in melee as well.  An Assassin will have mediocre magic skill,\
and poor magical defences.  He has moderately fast, strong pseudo-ID.\
His primary stats are Intelligence and Strength."

# OANGBANDTK
}

if {[variant ZANGBANDTK]} {

::msgcat::mcset $loc D_Warrior [join {
戦士は、直面する問題のほとんどを細切れに叩き切ることで解決す
るキャラクタです。が、時折退却して魔法の道具の世話になること
もあります。不運にも、高レベルなアイテムの多くは彼らが扱える
範囲を越えています。レベルが上がるにつれ、戦士はより武器の扱
いに熟練し、攻撃回数が増えていきます。技能が上昇すると敵を倒
す能力に自信を持ち恐怖に対する耐性を得ます。
} ""]

::msgcat::mcset $loc D_Mage [join {
メイジは魔法使いであり、その機知によって生き延びなければなり
ません。戦士のように、単純に切りまくることで道を開くことは望
めません。呪文書に加えて、メイジは助けになる魔法の道具を持ち
運ぶべきです。そのような道具の扱いをマスターすることは他のど
のような職業よりも優れています。また、メイジは敵にかけられた
魔法の効果にもっとも耐えることができるクラスです。ます。よい
賢さと器用さも助けになります。メイジがよい戦士になれないとい
うルールはありませんが、呪文こそがメイジの本当の力です。２つ
の注目すべき例外を除いて、メイジは手に防具を装備すべきではあ
りません。装備してしまうと、呪文を唱える能力を制限してしまい
ます。呪文を使う他のクラスと違って、メイジは魔法の領域から２
つの領域を自由に選ぶことができます。が、生命の領域ではプリー
ストほど上手くやれはしません。それを除けば、メイジは選択した
領域の全ての呪文を他のどのクラスよりも（ハイメイジは別です。
彼らはただ一つの領域に集中して努力を傾けるのです）うまく学び
、唱えることができます。
} ""]

::msgcat::mcset $loc D_Priest [join {
プリーストは高貴な力に使えることに専念したキャラクタです。彼
らは自身の神に仕えるためにダンジョンを探索し、もし宝を偶然手
にいれるようなことがあれば、それは彼が信仰する宗教の栄光とな
るのです。プリーストの主要な能力値は賢さです。この値が神への
祈りの成功率を決定します。プリーストは新しい祈りを神からの贈
り物という形で受け取るため、どれを学ぶのか自分で選ぶことはで
きません。プリーストは魔法の道具の使い方をよく知っています。
それらの道具は物の理への神の介入という形で働くと彼らは信じて
います。が、メイジほどうまく使えるわけではありません。プリー
ストはかけられた呪文によく対抗することができ、またそこそこの
戦士にもなれますが、刃のついた武器より鈍器を好みます。刃のつ
いた武器を装備すると不愉快な感覚に襲われ、その武器が神に祝福
されていない限り戦闘能力が落ちてしまいます。生命の領域を学ん
だ高レベルのプリーストは、このような武器に神の祝福を求める呪
文を得ることができます。Zangbandには2タイプのプリー
ストが存在します。通常のプリーストは、生命の領域を自分の主要
な領域として選択します。そして「暗き」プリーストはそのかわり
に暗黒の領域を選択します。プリーストは基本的に生命の領域への
嗜好があるため、生命を選択したプリーストは邪悪な同業者よりも
より早く、より上手く祈りを学ぶことができます。プリーストは第
2の領域を、生命、暗黒を除く5つの領域から選択することができ
、その領域についてもメイジほど効果的にではないにしろ、全ての
呪文を学ぶことができます。
} ""]

::msgcat::mcset $loc D_Rogue [join {
盗賊はその狡猾さで生き抜くことを好むキャラクタですが、肝心な
ときには戦闘で道を切り開くことができます、盗賊は罠やドアを見
つける能力に優れ、罠の解除や鍵開けに熟達しています。盗賊は高
い隠密行動能力を持ち、たくさんの怪物の群れのそばを起こすこと
なく通り抜けたり、忍び寄って先制攻撃することができます。盗賊
は逃げようとしている怪物にbackstabすることができます
。盗賊は戦士やパラディンより上手く魔法の道具を扱えますが、そ
の力に頼ることはできません。盗賊はまた４つの領域から一つを選
択し、いくつかの呪文を学ぶことができますが、メイジが使えるよ
うな強力な攻撃呪文は覚えられません。盗賊の主要な能力値は知能
と器用さですが、腕力や耐久力も重要です。Zandbandにお
ける盗賊には、いくつかのサブタイプが存在し、それは選択した魔
法の領域によってわけられます。普通なのはThiefタイプで、
彼らはその幅広い適応性のために秘術の領域を選択します。一方、
Burglarタイプはかわりに仕事を素早く効果的にこなせる仙
術の領域を選択します。Assassinタイプが暗黒の領域を特
に好むのは良く知られており、彼らはそのために恐れられています
。最後に "Card Shark" タイプはトランプの領域を選択し、
その驚くべき技倆を持ってカードを切るのです。盗賊はレベルが上
がるとスリングを扱う技能が上昇し、最終的には二回余計に弾を発
射する能力を得ます。
} ""]

::msgcat::mcset $loc D_Ranger [join {
レンジャーは戦士とメイジを合わせたようなクラスで、身の回りの
自然に特別な親しみを持っています。彼はより戦士であり、弓のよ
うな遠距離武器をもっとも巧く使えるクラスです。レンジャーはよ
い隠密行動能力、よい知覚能力、よい探索能力、よい魔法回避能力
を持ち、魔法の道具の使用にも長けています。レンジャーの主要な
能力値は呪文と弓の能力を左右する知能と器用さですが、戦士とし
ては腕力と耐久力も重要です。レンジャーが経験を積むと、彼のメ
イン武器である弓の技倆も上がり、より速く矢を射ることができま
す。クロスボウの技量も上昇しますが弓と同じほどという訳にはい
きません。残念なことに、レンジャーは本当にデュアルクラスであ
るため、レベルを上げるにはより多くの経験値が必要となります。
全てのレンジャーは自然の領域に精通しており、全ての呪文が使え
るようになります。彼らはこの領域の呪文をメイジと変わらぬ早さ
で覚えることができます。また、生命を除く領域から第2の領域を
選択することができますが、これについては学ぶ速度は遅く、もっ
とも高レベルの呪文のいくつかは覚えられません。
} ""]

::msgcat::mcset $loc D_Paladin [join {
パラディンは戦士とプリーストを合わせたクラスです。パラディン
はとてもよい戦士であり、戦士の次に優れていますが、遠距離武器
を扱うのは得意ではありません。パラディンには多くの能力が欠け
ています。隠密行動能力、知覚能力、探索能力、そして "magic"
"al devide" が苦手ですが、神への帰依によって魔法回避能
力はそこそこです。パラディンの主要な能力値は腕力、器用さ、耐
久力と賢さです。彼は戦闘と祈りの両方をうまくこなさなければな
らないのです。パラディンはプリーストよりもゆっくりと祈りを獲
得しますが、最終的にはもっとも強力な祈りを得ることができます
。しかし、消費MPと失敗率はプリーストより高いです。プリース
トと違い、パラディンは第2の領域を学ぶことはできません。パラ
ディンはデュアルクラスであるため、レベルを上げるにはより多く
の経験値が必要となります。パラディンには2タイプ存在します。
生命の領域を学ぶ者と、それとは反対に暗黒の領域に与する邪悪な
者です（「死の騎士」と呼ばれます）。通常のパラディンは生命の
領域以外の高レベル呪文書を破壊することで経験値を得ることがで
きます。一方暗黒騎士は他の領域に対し非常に寛容であり、唯一生
命の高レベル呪文書を破壊することで経験値を得ることができます
。レベルが上がると、パラディンは敵を打ち負かす能力と、神の御
加護に自信を持ちます。結果として、高いレベルで恐怖に対し耐性
を獲得します。
} ""]

::msgcat::mcset $loc D_Warrior-Mage [join {
魔法戦士はその名称が意味する通りのクラスであり、戦士とメイジ
の資質をあわせ持ちます。彼らの同業者であるレンジャーが自然の
魔法と生き抜くためのスキルに特化している一方、本当の魔法戦士
はどちらの世界でも一番になろうとしています。戦士としては普通
のメイジとは比べ物にならないほど優れています。魔法戦士は呪文
を使いたいが普通のメイジではすぐに死んでしまうようなプレイヤ
ーにお薦めできます。しかし、その優れた能力はタダで手にはいる
わけではなく、魔法戦士はほかのどのクラスよりもレベルを上げる
のに多くの経験値が必要となります。魔法戦士は秘術の魔法を持っ
てゲームを開始し、さらにもう1領域を自由に選択することができ
ます。彼らは通常のメイジほどは早く呪文を覚えられませんが、最
終的には両領域の全ての呪文を覚えることができます。このように
、秘術の魔法を評価しているプレイヤーにとって、このクラスは大
変に有用な選択です。
} ""]

::msgcat::mcset $loc D_Chaos-Warrior [join {
混沌の戦士は恐るべきカオスの魔王の使いとして恐れられる存在で
す。全ての混沌の戦士はパトロンとなる悪魔を持ち、レベルが上が
る度にパトロンからの報酬を得ることがあります。彼は治療してく
れたり、こちらを変化させたり、能力値を上げてくれるかもしれま
せんし、もしかしたら強力な武器を与えてくれるかもしれません。
何らかの理由でいらいらしていたら、パトロンは回りに怪物達を出
現させるかもしれませんし、能力値や装備を奪うかも知れません。
もしくは単にこちらを無視するだけかもしれません。カオスの魔王
は無秩序で予測のつかない存在です。報酬の正確なタイプはパトロ
ンとなる悪魔（違う悪魔は異なる報酬を与えます）と偶然に依存し
ます。混沌の戦士はだれもが予想する通りカオスの魔法に精通して
います。彼らは他の領域に関心を示しません。彼らはカオスの全呪
文を覚えます。レベルが上がると、彼らは敵を打ち負かす能力に自
信を深め、レベル40で恐怖への耐性を獲得します。それに加えて
、カオスの魔王への長年の奉仕の結果として、あるレベルでカオス
攻撃への耐性を獲得します。
} ""]

::msgcat::mcset $loc D_Monk [join {
修行僧は他のクラスとは著しく異なるクラスです。彼らは他のクラ
ス同様武器と防具を使えますが、マーシャルアーツの訓練を積んで
いるため、武器、防具なしでより強力な存在となります。高レベル
では、必要な耐性を身につけるためある種の防具を装備する必要が
ありますが、もしあまりに重すぎる防具を装備してしまうと、その
体術に深刻な妨げとなります。レベルが上がると、彼らは新しい強
力な攻撃法を学び、さらに攻撃回数が増えます。彼らの防御能力も
同様に上昇します。幸運なことに、修行僧が戦闘に差し支えない範
囲で装備できる防具の総量は、経験とともに上昇します。それに加
え、修行僧の敏捷性は高レベルで麻痺への耐性を与えますが、それ
は防具が彼の動きを妨げていない場合のみです。修行僧はレベルが
上がるとともにより素早く動けるようになり攻撃回数も増えます。
異なる宗派に属する修行僧はそれぞれ違う領域の魔法に専念します
。典型的な修行僧は自然の調和に興味を抱き、自然の魔法を学びま
す。理想主義の修行僧は生命の魔法を選択し、隣人のためになろう
と努力します。しかし暗黒の修行僧は暗黒の魔法を学びます。修行
僧は最終的に全ての呪文を得ることができます。
} ""]

::msgcat::mcset $loc D_Mindcrafter [join {
超能力者は魔法のかわりにその精神の力を使う唯一のクラスです。
この力は超能力者独特のもので、単に超感覚的な力から他人の精神
的な支配まで変わります。彼らの力はある種の訓練によって開発さ
れるものなので、超能力者は力を使うのに呪文書を必要としません
。使える力は単純にキャラクタのレベルによって決まります。超能
力者の主要な能力値は賢さであり、この値が精神力をどの程度うま
く使えるかを決定します。そして戦闘においては、彼はおおざっぱ
に言ってプリーストと同程度の能力を持っています。しかし、プリ
ーストと違い彼らは刃のついた武器をしようすることによるペナル
ティを受けません。超能力者の力は魔法に似ているかもしれません
が、これは厳密ではありません。それは精神の力であり、通常の魔
法とは独立しています。結果として、超能力者は「魔法」には興味
を示さず、呪文や祈りは覚えません。レベルが上がると、彼らは自
分の身体を精神でコントロールすることができるようになります。
レベルが上がると恐怖をコントロールし、混乱への耐性を獲得しま
す。また精神力の訓練によって、レベルがあがると賢さを下げられ
なくなり、テレパシーを獲得します。
} ""]

::msgcat::mcset $loc D_High-Mage [join {
ハイメイジは一つの領域に特化し、その領域を通常のメイジよりは
るかに深く学んだメイジです。１つの領域に特化したおかげで、彼
らは自らが選択した領域の呪文を唱える際の消費MP、最低レベル
、失敗率で相当な恩恵を受けます。ハイメイジの主要な能力値は知
能であり、この値が呪文を唱える能力を決定します。よい賢さと器
用さもまた助けとなります。ハイメイジがよい戦士になれないとい
うルールはありませんが、呪文こそがメイジの本当の力です。２つ
の注意すべき例外を除いて、彼らは手に防具を装備するのは避ける
べきです。装備してしまうと、呪文を唱える能力に制限を受けます
。ハイメイジは自由に魔法の領域を一つ選択できますが、生命を選
択してもプリーストほどうまくはなれないことには注意すべきです
。
} ""]

#
# Description of magic realms
#

::msgcat::mcset $loc D_Life [join {
生命は「よい」魔法です。これは治療や防御に非常に頼っていますが、攻撃呪文もいく
つか持っています。それらは悪魔の汚れた手先に対抗するためによく使われます。
噂によれば、プリーストやパラディンをあらゆる攻撃から完全に守る
秘密の高位呪文があるそうです。
} ""]

::msgcat::mcset $loc D_Sorcery [join {
仙術は「メタ」領域であり、装備強化や総合的な呪文を含んでいま
す。優れた防御呪文(逃走のためのテレポートや不可侵のバリアさ
えも)、戦闘で優位に立つための呪文(スロウ・モンスター、スピ
ード、パニック・モンスター)、そして最も重要な情報を得るため
の呪文、すなわち通常の感知呪文や鑑定呪文に加え、真・鑑定と呼
ばれる*鑑定*の効果を持つものなどです。希少な魔法書には、ア
イテムに魔法をかけたり、不要なアイテムを金に変える呪文さえあ
ります。しかし仙術には一つ弱点があります。直接攻撃用の呪文を
持っていないのです。
} ""]

::msgcat::mcset $loc D_Arcane [join {
仙術よりもさらに、秘術魔法は汎用的な領域です。全ての領域から
有用な呪文を集めるべく作られたもので、*鑑定*が無いことを除
けば概ね成功していると言えるでしょう。これは欠点でもあります
。秘術はダンジョンの探検に必要な「補助」呪文をそろえています
が、超強力な高位呪文はありません。結果として全ての秘術の魔法
書は街で買うことができます。また「特化された」領域、つまり秘
術以外の領域では同じ呪文がより低いレベルかつ低MPで使用可能
であることも注意すべき点です。それゆえ秘術魔法は唯一の選択と
しては薦められません。しかし他の領域に欠けている補助呪文を補
うにはいい領域です。
} ""]

::msgcat::mcset $loc D_Trump [join {
トランプ魔法は独立な力のように見えます。しかしカオス魔法との
関係の記述はいくつかの場所で見られます。カオス魔法の予測不能
な副作用は無いのですが、いくつかの魔法はその効果がいくらかラ
ンダムになっています。そのような呪文の一つが「シャッフル」で
す。トランプの魔法書は実際にはカードのデッキからなり、シャッ
フルの魔法はそのデッキからランダムに一枚のカードを選びます。
魔法の効果は選んだカードで決まり、いつも良いものであるとは限
りません。アンバーの宇宙では、トランプの扉は重要な移動手段で
もあります。実際トランプ魔法は素晴らしいテレポート呪文の数々
を持っています。トランプの扉はモンスターを召還するのにも使え
るため、トランプ魔法は召還呪文も素晴らしいものが揃っています
。しかし全てのモンスターが召還されるのを喜ぶわけではありませ
ん。100%忠誠なモンスターは幽霊の下僕のみで、彼らは自分の
意志を持っていません(ただしひどい扱いをすれば敵対意志をもつ
でしょう)。
} ""]

::msgcat::mcset $loc D_Nature [join {
自然魔法に頼る場合、序盤は限られた防御、感知、治癒の呪文しか
ないので困難でしょう。しかしレベルが上がれば役に立つ攻撃呪文
が使用でき、さらに「自然の脅威」と呼ばれる非常に希少な魔法書
を入手できればさらに強力な攻撃呪文が使えます。自然はさらに薬
草治療の呪文を持っています。これは生命以外の領域では唯一の強
力な回復呪文です。
} ""]

::msgcat::mcset $loc D_Chaos [join {
カオスの魔法より制御が困難で、予測のできない魔法はほとんどあ
りません。カオスは破壊の本質そのものであり、カオスの呪文は想
像できる最も恐るべき破壊兵器です。マジック・ミサイルやファイ
ア・ボルトに始まり、中レベルのファイア・ボールやドゥーム・ボ
ルト、そしておそるべきログルス発動、魔力の嵐、虚無召来の呪文
に至るまでカオスは他の追随を許さない攻撃魔法を提供します。術
者はカオスの本質的な力を召喚することにより敵や自分自身までも
変化させることができますが、その他には防御の呪文はありません
。カオスの呪文は簡単に逆流し予測不能な効果を及ぼすことに注意
してください。特にバージョン2.1.0以降はそうで、カオスの
力を利用しようとする愚かな術者は簡単に歪められ、おそるべきカ
オスの申し子に変形させられます。
} ""]

::msgcat::mcset $loc D_Death [join {
暗黒の魔法における黒魔術ほど邪悪なカテゴリーはありません。こ
れらの呪文は比較的学ぶのが困難ですが、高レベルになると術者に
生物とアンデッドを自由に操る能力を与えます。術者は毒、吸血、
死の呪文、また地獄の炎までも操ることができますが、強力な呪文
の多くは術者の血を触媒として必要とするので、術者は呪文を唱え
る過程で傷つくことがしばしばあります。もし伝説の書ネクロノミ
コンを発見できれば、非常に強力なパワーを得ることができるでし
ょうが、大きな代償を支払わねばならないでしょう。この呪わしき
書を読んで正気を保っていた人間は数えるほどしかいません。
} ""]

# ZANGBANDTK
}



# Autoroller
::msgcat::mcset $loc "Autoroller" オートローラー
::msgcat::mcset $loc "> Hit any key to abort <" 何かキーを押すと中止します

::msgcat::mcset $loc "    Strength:" 腕力:
::msgcat::mcset $loc "Intelligence:" 知力:
::msgcat::mcset $loc "      Wisdom:" 賢さ:
::msgcat::mcset $loc "   Dexterity:" 器用:
::msgcat::mcset $loc "Constitution:" 耐久:
::msgcat::mcset $loc "    Charisma:" 魅力:

::msgcat::mcset $loc "Round: %d" "回数: %d"
::msgcat::mcset $loc "(Max of %s)"
::msgcat::mcset $loc "Total: %2d/%d"

::msgcat::mcset $loc "Enter minimum desired stats for the 'auto-roller'" \
	能力値を設定してください。
::msgcat::mcset $loc D_auto-roller1 [join {
	オートローラーは能力値が設定値を越えるキャラクターを
	作成しようと試みます。設定値が高すぎると作成できない可能性があります。
	一部あるいは全部の最小値を設定無しにしても大丈夫です。
} ""]

::msgcat::mcset $loc D_auto-roller2 [join {
	オートローラーは能力値が設定値を越えるキャラクターを
	作成しようと試みます。
	一部あるいは全部の最小値を設定無しにしても大丈夫です。
} ""]

::msgcat::mcset $loc Prompt_Points \
	"キャラクターの能力値を振り分けて下さい。"
if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
	set amount 100
}
if {[variant OANGBANDTK]} {
	set amount 50
}
::msgcat::mcset $loc Info_Points \
	"48ポイントまでを振り分けて下さい。余ったポイントは１ポイントにつき$amount ゴールドの所持金に変換されます。"
::msgcat::mcset $loc "(Cost %d)" "(%d使用)"

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc GHOSTLY "Pass through walls"
::msgcat::mcset $loc HURT_FIRE "Hurt by fire"
::msgcat::mcset $loc HURT_LITE "Hurt by light"
}

if {[variant OANGBANDTK]} {
# player_race.flags_special
::msgcat::mcset $loc SWORD_SKILL "Skill with swords"
::msgcat::mcset $loc POLEARM_SKILL "Skill with polearms"
::msgcat::mcset $loc HAFTED_SKILL "Skill with hafted weapons"
::msgcat::mcset $loc SLING_SKILL "Skill with slings"
::msgcat::mcset $loc BOW_SKILL "Skill with bows"
::msgcat::mcset $loc XBOW_SKILL  "Skill with crossbows"
::msgcat::mcset $loc SWORD_UNSKILL "Clumsy with swords"
::msgcat::mcset $loc POLEARM_UNSKILL "Clumsy with polearms"
::msgcat::mcset $loc HAFTED_UNSKILL "Clumsy with hafted weapons"
::msgcat::mcset $loc SLING_UNSKILL "Clumsy with slings"
::msgcat::mcset $loc BOW_UNSKILL "Clumsy with bows"
::msgcat::mcset $loc XBOW_UNSKILL "Clumsy with crossbows"
::msgcat::mcset $loc HARDY "Recover quicker from poison and cuts</li><li>Resist Black Breath"
::msgcat::mcset $loc HUNGRY "Needs food more often"
::msgcat::mcset $loc DIVINE "Recover quicker from hallucination, blindness, paralysis, confusion, fear, slowness, poison, stun and cuts</li><li>Score reduced by half"
::msgcat::mcset $loc SHADOW "Take 1/3 extra damage from light</li><li>Aggravate monsters disabled with -3 to stealth"
::msgcat::mcset $loc WOODEN "Feather falling disabled</li><li>Take 1/3 extra damage from fire</li><li>Powerful digging with hands</li><li>Strength and constitution increased a point at levels 26, 41 and 46</li><li>Dexterity decreased a point at levels 26, 41 and 46"
}

# Misc text in the character info window
::msgcat::mcset $loc prompt-name "Enter a name for your character:"
::msgcat::mcset $loc prompt-finish "Type 's' to restart, Escape to accept"
::msgcat::mcset $loc life-rating "Life Rating: %d/100"

