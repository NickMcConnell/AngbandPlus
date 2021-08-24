/*
 * Copyright (c) 2014 Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <QApplication>

#include "src/qt_mainwindow.h"
#include "src/npp.h"



int main(int argc, char *argv[])
{
    //first figure out which game mode we are playing
    game_mode = 0;

    QApplication app(argc, argv);
    QIcon main_icon(":/icons/lib/icons/nppicon.ico");
    app.setWindowIcon(main_icon);

    app.setApplicationName("NPP Games");

    MainWindow *main_window = new MainWindow;

    main_window->setGeometry(main_window->win_geometry);
    if (main_window->win_maximized) main_window->showMaximized();
    else main_window->show();
    return app.exec();
}
