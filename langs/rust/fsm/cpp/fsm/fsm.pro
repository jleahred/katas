TEMPLATE = app
CONFIG += console c++17
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
        main.cpp \
    fsm_login_base.cpp \
    fsm_login.cpp

HEADERS += \
    fsm_login_base.h \
    fsm_login.h
