set loc ja

# Statusbar
::msgcat::mcset $loc status-player "%s, the %s %s"
		
#Flag titles
::msgcat::mcset $loc STR 腕力±
::msgcat::mcset $loc INT 知力±
::msgcat::mcset $loc WIS 賢さ±
::msgcat::mcset $loc DEX 器用±
::msgcat::mcset $loc CON 耐久±
::msgcat::mcset $loc CHR 魅力±
::msgcat::mcset $loc SUST_STR 腕力維持
::msgcat::mcset $loc SUST_INT 知力維持
::msgcat::mcset $loc SUST_WIS 賢さ維持
::msgcat::mcset $loc SUST_DEX 器用さ維持
::msgcat::mcset $loc SUST_CON 耐久力維持
::msgcat::mcset $loc SUST_CHR 魅力維持
::msgcat::mcset $loc IM_ACID 免疫・酸
::msgcat::mcset $loc IM_ELEC 免疫・電撃
::msgcat::mcset $loc IM_FIRE 免疫・火炎
::msgcat::mcset $loc IM_COLD 免疫・冷気
::msgcat::mcset $loc RES_ACID 耐酸
::msgcat::mcset $loc RES_ELEC 耐電
::msgcat::mcset $loc RES_FIRE 耐火
::msgcat::mcset $loc RES_COLD 耐冷
::msgcat::mcset $loc RES_POIS 耐毒
::msgcat::mcset $loc RES_FEAR 耐恐怖
::msgcat::mcset $loc RES_LITE 耐閃光
::msgcat::mcset $loc RES_DARK 耐暗黒
::msgcat::mcset $loc RES_BLIND 耐盲目
::msgcat::mcset $loc RES_CONFU 耐混乱
::msgcat::mcset $loc RES_SOUND 耐轟音
::msgcat::mcset $loc RES_SHARD 耐破片
::msgcat::mcset $loc RES_NEXUS 耐因果混乱
::msgcat::mcset $loc RES_NETHR 耐地獄
::msgcat::mcset $loc RES_CHAOS 耐カオス
::msgcat::mcset $loc RES_DISEN 耐劣化
::msgcat::mcset $loc SLOW_DIGEST 遅消化
::msgcat::mcset $loc FEATHER 羽落下
::msgcat::mcset $loc LITE 永久光源
::msgcat::mcset $loc REGEN 急速回復
::msgcat::mcset $loc TELEPATHY テレパシー
::msgcat::mcset $loc SEE_INVIS 透明体視認
::msgcat::mcset $loc FREE_ACT 麻痺知らず
::msgcat::mcset $loc HOLD_LIFE 生命力保持
::msgcat::mcset $loc STEALTH 隠密
::msgcat::mcset $loc SEARCH 探索
::msgcat::mcset $loc INFRA 赤外線視力
::msgcat::mcset $loc TUNNEL 掘削
::msgcat::mcset $loc SPEED 加速
::msgcat::mcset $loc BLOWS 打撃回数
::msgcat::mcset $loc SHOTS 射撃回数
::msgcat::mcset $loc MIGHT 強力射撃
::msgcat::mcset $loc SLAY_ANIMAL 動物倍打
::msgcat::mcset $loc SLAY_EVIL 邪悪存在倍打
::msgcat::mcset $loc SLAY_UNDEAD アンデッド倍打
::msgcat::mcset $loc SLAY_DEMON デーモン倍打
::msgcat::mcset $loc SLAY_ORC オーク倍打
::msgcat::mcset $loc SLAY_TROLL トロル倍打
::msgcat::mcset $loc SLAY_GIANT ジャイアント倍打
::msgcat::mcset $loc SLAY_DRAGON ドラゴン倍打
::msgcat::mcset $loc KILL_DRAGON ドラゴン倍倍打
::msgcat::mcset $loc BRAND_ACID 酸属性攻撃
::msgcat::mcset $loc BRAND_ELEC 電気属性攻撃
::msgcat::mcset $loc BRAND_FIRE 火属性攻撃
::msgcat::mcset $loc BRAND_COLD 冷気属性攻撃
::msgcat::mcset $loc IMPACT 地震
::msgcat::mcset $loc TELEPORT テレポート
::msgcat::mcset $loc AGGRAVATE 反感
::msgcat::mcset $loc DRAIN_EXP 経験値吸収
::msgcat::mcset $loc BLESSED 祝福
::msgcat::mcset $loc LIGHT_CURSE 呪い
::msgcat::mcset $loc HEAVY_CURSE 重い呪い
::msgcat::mcset $loc PERMA_CURSE 永遠の呪い

if {[variant ZANGBANDTK]} {
# Flags spelled differently
::msgcat::mcset $loc RES_CONF 耐混乱
::msgcat::mcset $loc RES_SHARDS 耐破片
::msgcat::mcset $loc RES_NETHER 耐地獄
::msgcat::mcset $loc XTRA_SHOTS 射撃回数
::msgcat::mcset $loc XTRA_MIGHT 強力射撃
::msgcat::mcset $loc CURSED 呪い

# Extra flags
::msgcat::mcset $loc RACE_IM_CUT 免疫・傷
::msgcat::mcset $loc RACE_IM_DARK 免疫・暗黒
::msgcat::mcset $loc RACE_IM_STUN 免疫・朦朧
::msgcat::mcset $loc RACE_RES_SANITY 免疫・狂気
::msgcat::mcset $loc REFLECT ボルト反射
::msgcat::mcset $loc RACE_EAT_NETHER 暗黒吸収
::msgcat::mcset $loc BRAND_POIS 毒属性攻撃
::msgcat::mcset $loc SH_FIRE 炎のオーラ
::msgcat::mcset $loc SH_ELEC 電気のオーラ
::msgcat::mcset $loc VORPAL ヴォーパル
::msgcat::mcset $loc CHAOTIC カオス的効果
::msgcat::mcset $loc VAMPIRIC 吸血
::msgcat::mcset $loc WRAITH 幽体化
::msgcat::mcset $loc NO_MAGIC 反魔法
::msgcat::mcset $loc NO_TELE 反テレポート
::msgcat::mcset $loc TY_CURSE 上古の忌まわしい呪い
}

