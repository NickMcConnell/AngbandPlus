set loc ja

::msgcat::mcset $loc copyright "%s: Copyright (c) 2009 Tim Baker"

::msgcat::mcset $loc original-a "Based on Angband: Copyright (c) 2000 R. Ruehlmann"
::msgcat::mcset $loc original-k "Based on KAngband: Copyright (c) 2000 John I'anson-Holton"
::msgcat::mcset $loc original-o "Based on OAngband: Copyright (c) 2000 Bahman Rabii"
::msgcat::mcset $loc original-z "Based on ZAngband (c) the ZAngband Dev Team"

::msgcat::mcset $loc newsgroup "Newsgroup: rec.games.roguelike.angband"
::msgcat::mcset $loc email "Send comments to omnibandtk@shaw.ca"

# Fonts
if {[Platform unix]} {
::msgcat::mcset $loc font,sys "{mincho}"
::msgcat::mcset $loc font,fixed "{mincho}"
}

if {[Platform windows]} {
::msgcat::mcset $loc font,sys "{ＭＳ Ｐゴシック}"
::msgcat::mcset $loc font,fixed "{ＭＳ Ｐゴシック}"
}

# Startup Window: buttons
::msgcat::mcset $loc New 新規
::msgcat::mcset $loc Open 再開
::msgcat::mcset $loc Setup 設定
::msgcat::mcset $loc Quit 終了

# Choose Game Window: window title
::msgcat::mcset $loc choose-title キャラクタ選択

# Choose Game Window: prompt
::msgcat::mcset $loc choose-prompt [join {
lib\\saveディレクトリのファイルからキャラクタを選んでください。
他のディレクトリから選ぶには\"参照...\"を押してください。
} ""]

# Choose Game Window: column titles
::msgcat::mcset $loc choose-Name 名前
::msgcat::mcset $loc choose-Version バージョン
::msgcat::mcset $loc choose-Description 内容

# Choose Game Window: savefile info
::msgcat::mcset $loc choose-savefile "Level %d %s %s, Depth %d/%d"

# Choose Game Window: buttons
::msgcat::mcset $loc choose-Open 開く

# Choose Game Window: misc
::msgcat::mcset $loc choose-file 選択ファイル
::msgcat::mcset $loc scan-title "セーブファイル検索中..."
::msgcat::mcset $loc scan-file "\"%s\"を読んでいます"
::msgcat::mcset $loc scan-display "表示中"

# Choose Game Window: tabs
::msgcat::mcset $loc choose-tab-living "Living"
::msgcat::mcset $loc choose-tab-dead "Dead"
::msgcat::mcset $loc choose-tab-backup "Backup"

# Choose Game Window: backup dialog
::msgcat::mcset $loc title-backup "Restore backup file"
::msgcat::mcset $loc msg-backup "The savefile \"%s\" appears to be a backup."
::msgcat::mcset $loc backup-choice1 "Rename the file to \"%s\"."
::msgcat::mcset $loc backup-choice2 "Choose a new name for the file."
::msgcat::mcset $loc backup-choice3 "Use the file without renaming it."

# Choose Game Window: delete dialog
::msgcat::mcset $loc choose-delete-title "Delete savefile"
::msgcat::mcset $loc choose-delete-msg "Really delete this savefile?\n\n%s"

# Setup Window: window title
::msgcat::mcset $loc setup-title 設定

# Setup Window: tabs
::msgcat::mcset $loc setup-Icon グラフィック
::msgcat::mcset $loc setup-Music ＢＧＭ
::msgcat::mcset $loc setup-Sound 効果音
::msgcat::mcset $loc setup-Variant Variant

# Setup Window: Icons page
::msgcat::mcset $loc icons-prompt "グラフィックを選択してください:"
::msgcat::mcset $loc adam16 "Adam Bolt 16x16"
::msgcat::mcset $loc adam24 "Adam Bolt 24x24"
::msgcat::mcset $loc adam32 "Adam Bolt 32x32"
::msgcat::mcset $loc adam16-shape "Adam Bolt (Shape) 16x16"
::msgcat::mcset $loc adam24-shape "Adam Bolt (Shape) 24x24"
::msgcat::mcset $loc adam32-shape "Adam Bolt (Shape) 32x32"
::msgcat::mcset $loc adam16+orig "Adam Bolt + Original Features 16x16"
::msgcat::mcset $loc adam32+orig "Adam Bolt + Original Features 32x32"
::msgcat::mcset $loc ascii "Ascii"
::msgcat::mcset $loc ascii16+adam "Ascii + Adam Bolt Features 16x16"
::msgcat::mcset $loc ascii24+adam "Ascii + Adam Bolt Features 24x24"
::msgcat::mcset $loc ascii32+adam "Ascii + Adam Bolt Features 32x32"
::msgcat::mcset $loc ascii16+adam-shape "Ascii + Adam Bolt Features (Shape) 16x16"
::msgcat::mcset $loc ascii24+adam-shape "Ascii + Adam Bolt Features (Shape) 24x24"
::msgcat::mcset $loc ascii32+adam-shape "Ascii + Adam Bolt Features (Shape) 32x32"
::msgcat::mcset $loc ascii+dg32 "Ascii + David E. Gervais 32x32"
::msgcat::mcset $loc ascii16+orig "Ascii + Original Features 16x16"
::msgcat::mcset $loc ascii32+orig "Ascii + Original Features 32x32"
::msgcat::mcset $loc dg32 "David E. Gervais 32x32"
::msgcat::mcset $loc dg32+iso "Isometric David E. Gervais 32x32"
::msgcat::mcset $loc original32 "Original 32x32"

# Setup Window: Music page
::msgcat::mcset $loc music-prompt [join {
サウンドカードがない場合や、ゲーム開始数秒でクラッシュする
場合は OFF にしてください。
} ""]
::msgcat::mcset $loc music-use "BGM を使用"
::msgcat::mcset $loc music-lib BGM用ライブラリ:
::msgcat::mcset $loc music-desc 詳細:
::msgcat::mcset $loc music-no-libs "BGM用ライブラリがありません。"

# Platform: windows
::msgcat::mcset $loc music-use-bass "BASS"
::msgcat::mcset $loc music-prompt-bass \
"DirectX 3 以上が必要です。"
::msgcat::mcset $loc music-use-fmod "FMOD"
::msgcat::mcset $loc music-prompt-fmod \
"Play music using FMOD."
::msgcat::mcset $loc music-use-midas "MIDAS"
::msgcat::mcset $loc music-prompt-midas \
"MIDAS デジタル・オーディオ・システム"

# Setup Window: Sound page
::msgcat::mcset $loc sound-prompt \
	"サウンドカードがない場合オンにしないでください。"
::msgcat::mcset $loc sound-use "サウンド使用"
::msgcat::mcset $loc sound-lib サウンド用ライブラリ:
::msgcat::mcset $loc sound-desc 詳細:
::msgcat::mcset $loc sound-no-libs "サウンド用ライブラリがありません。"

# Platform: windows
::msgcat::mcset $loc sound-use-directsound "DirectSound 使用"
::msgcat::mcset $loc sound-prompt-directsound [join {
DirectSound は高速に音を鳴らし、任意の周波数で
再生できます。こちらがおすすめです。
} ""]
::msgcat::mcset $loc sound-use-bass "BASS (samples)"
::msgcat::mcset $loc sound-prompt-bass \
        "DirectX 3 以上が必要です。"
::msgcat::mcset $loc sound-use-bass-stream "BASS (streams)"
::msgcat::mcset $loc sound-prompt-bass-stream \
        "DirectX 3 以上が必要です。"
::msgcat::mcset $loc sound-use-fmod "FMOD"
::msgcat::mcset $loc sound-prompt-fmod \
	"Play sounds using the FMOD library."
::msgcat::mcset $loc sound-use-wavemix "WaveMix 使用"
::msgcat::mcset $loc sound-prompt-wavemix [join {
これは遅く、かつ11KHzでしか音を再生できません。
DirectX がインストールされてない場合のみこちらを選択してください。
} ""]

# Platform: unix
::msgcat::mcset $loc sound-use-openal "OpenAL"
::msgcat::mcset $loc sound-prompt-openal \
	"OpenAL ライブラリを使用"
::msgcat::mcset $loc sound-use-sdl "SDL Mixer"
::msgcat::mcset $loc sound-prompt-sdl \
	"SDL Mixer ライブラリを使用"

# Setup Window: Variant page
::msgcat::mcset $loc variant-always "Always choose this variant"

# UpgradeTool
::msgcat::mcset $loc upgrade-tool-title "設定ファイルのコピー"
::msgcat::mcset $loc upgrade-tool-prompt [join {
アップグレード・ツールを使うと今まで使用していた%sの
古いバージョンのセーブファイルや設定ファイルを引き継いで
使うことができます。初めてこのゲームをする場合は必要
ありません。\n\nアップグレード・ツールを起動しますか?
} ""]

