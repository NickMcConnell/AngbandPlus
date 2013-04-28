set loc ja

# Window title
::msgcat::mcset $loc Equipment 装備
::msgcat::mcset $loc Inventory 持ち物
::msgcat::mcset $loc Floor 床上
::msgcat::mcset $loc Items アイテム
if {[variant OANGBANDTK]} {
::msgcat::mcset $loc Quiver
}

# Window
::msgcat::mcset $loc Inscription: 銘:
::msgcat::mcset $loc Displayed 表示合計
::msgcat::mcset $loc Total 合計
::msgcat::mcset $loc Threshold 通常行動限界
::msgcat::mcset $loc Limit 限界
::msgcat::mcset $loc "%d item" "%d個のアイテム"
::msgcat::mcset $loc "%d items" "%d個のアイテム"
::msgcat::mcset $loc weight1_lb "%s lb"
::msgcat::mcset $loc weight2_lb "%s/%s lb"
::msgcat::mcset $loc weight3_lb "%s/%s/%s lb"
::msgcat::mcset $loc weight1_kg "%s kg"
::msgcat::mcset $loc weight2_kg "%s/%s kg"
::msgcat::mcset $loc weight3_kg "%s/%s/%s kg"

# Usage
::msgcat::mcset $loc Activate 発動する
::msgcat::mcset $loc Remove 外す
::msgcat::mcset $loc Browse 開く
::msgcat::mcset $loc Fire 発射する
::msgcat::mcset $loc Refuel 補充する
::msgcat::mcset $loc Eat 食べる
::msgcat::mcset $loc Drink 飲む
::msgcat::mcset $loc Read 読む
::msgcat::mcset $loc Jam はさむ
::msgcat::mcset $loc Use 使う
::msgcat::mcset $loc Zap 使う
::msgcat::mcset $loc Aim 使う
::msgcat::mcset $loc Wield 装備する

# Popup
::msgcat::mcset $loc *Destroy* *壊す*
::msgcat::mcset $loc Sell 売る
::msgcat::mcset $loc Drop 落とす
::msgcat::mcset $loc "Drop One"
::msgcat::mcset $loc "Drop Some"
::msgcat::mcset $loc "Drop All"
::msgcat::mcset $loc Throw
::msgcat::mcset $loc "Select This Item" このアイテムを選択
::msgcat::mcset $loc Inspect 調べる
::msgcat::mcset $loc Inscribe 銘を刻む
::msgcat::mcset $loc Uninscribe 銘を消す

# Options
::msgcat::mcset $loc "Show Flavors" 外見も表示
::msgcat::mcset $loc "Show Icons" アイコン表示
::msgcat::mcset $loc "Show Labels" ラベル表示
::msgcat::mcset $loc "Show Weights" 重量表示
::msgcat::mcset $loc "Use Metric" メートル法
::msgcat::mcset $loc "Always On Top" 常に一番上に表示
::msgcat::mcset $loc "Graphics Mode" グラフィックモード
::msgcat::mcset $loc "List Mode" 一覧モード

# Statusbar
::msgcat::mcset $loc status-total-threshold-limit 合計/通常行動限界/限界
::msgcat::mcset $loc status-weight-equip "Weight of equipment items"
::msgcat::mcset $loc status-number-inven "Number of inventory items"
::msgcat::mcset $loc status-weight-inven "Weight of inventory items"

