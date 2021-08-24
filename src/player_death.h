#ifndef PLAYER_DEATH_H
#define PLAYER_DEATH_H

#include <QTime>
#include <QDate>
#include "src/npp.h"
#include "src/store.h"
#include <QPointer>



class PlayerDeathDialog : public QDialog
{
    Q_OBJECT

public:
    explicit PlayerDeathDialog(void);

private slots:
    void death_player_info(void);
    void death_inven_info(void);
    void death_home_inven(void);
    void death_messages(void);
    void death_file_dump(void);
    void death_screenshot(void);
    void death_scores(void);
    void death_examine(void);
    void death_notes(void);
    void death_spoilers(void);

};

#endif // PLAYER_DEATH_H
