#!/usr/bin/env python

# uboat.py --- generate u-boat-death messages, patterned after Iron Coffins

# Author: Brian W. Fitzpatrick <fitz@red-bean.com>
# Original elisp author: Bob Manson <manson@cygnus.com>
# Original elisp maintainer: Noah Friedman <friedman@prep.ai.mit.edu>
# Keywords: extensions
# Created: 2010-04-21
# Originally created: 1995-08-02

# Bozoup(P) 1995 The Bozo(tic) Softwar(e) Founda(t)ion, Inc.
# See the BOZO Antipasto for further information.
# If this is useful to you, may you forever be blessed by the Holy Lord
# Patty.  AT&T you will.

# Commentary:

# Bob Manson originally wrote this program in ksh,
# then later converted it to emacs lisp.
# Mark Welch contributed additional items to the dictionary.
# Noah Friedman rewrote parts of the engine.
# Brian W. Fitzpatrick rewrote it in Python

import random

class Uboat():
    def __init__(self):
        self._uboat_message = [
            ['*verb', 'BY', '*device.', '*sinkage.'],
            ['*device', '*verb.', '*sinkage.'],
            ['*verb', 'BY', '*device.', '*condition.', '*sinkage.'],
            ['*dropdeviced', 'BY', '*number', '*ships.', '*sinkage'],
            ['*verb', 'BY', '*device', '*position.', '*sinkage.'],
            ['*verb', 'BY', '*device.', '*dropdevices.', '*sinkage.'],
        ]

        self._uboat_remedy = [
            'VALIUM NOZZLE',
            'PLOT DEVICE',
        ]

        self._uboat_sinkage = [
            'SINKING',
            'SINKING',
            'SINKING',
            'SINKING',
            'SINKING',
            'SINKING',
            ['ENGAGING', '*remedy'],
            'EXPLODING',
            'LEAVE BOAT',
        ]

        self._uboat_verb = [
            'ATTACKED',
            'BOMBED',
            'TORPEDOED',
            'ANNOYED',
        ]

        self._uboat_adjective = [
            'TINY',
            'SMALL',
            'ENORMOUS',
        ]

        self._uboat_device = [
            'AIRCRAFT',
            'CORVETTE',
            'ATOMIC BOMB',
            'DESTROYER',
            'SALESMEN',
            ['*adjective', 'GNATS'],
            'IRC SERVER',
            'NETSCAPE',
            'SPACE_TIME VORTEX',
        ]

        self._uboat_number = [
            'TWO',
            'THREE',
            'FOUR',
            'FIVE',
            'EIGHT',
            'HUNDREDS OF',
            'THOUSANDS OF',
            'MANY',
        ]

        self._uboat_dropdevices = [
            'DEPTH CHARGES',
            'TORPEDOS',
            'WEB PAGES',
        ]

        self._uboat_dropdeviced = [
            'DEPTH CHARGED',
            'TORPEDOED',
            'BLOWN UP',
            'CLICKED',
        ]

        self._uboat_ships = [
            'CORVETTES',
            'CARRIERS',
            'AIRPLANES',
            'DESTROYERS',
            'ATOMIC BOMBS',
            'BROKEN IRC SERVERS',
            'CRASHING WEB BROWSERS',
        ]

        self._uboat_unable_verb = [
            'DIVE',
            'SURFACE',
            'STAY AFLOAT',
            'USE RADIO',
        ]

        self._uboat_condition = [
            ['UNABLE TO', '*unable_verb'],
            'CREW UNMOTIVATED',
            'CAPTAIN INTOXICATED',
        ]

        self._uboat_position = [
            '57W 24N',
            '12W 23N',
            '87E 19S',
            '12N 12W',
            '29W 32S',
            '14E 33N',
            '122W 41N',
        ]


    def _parse_template(self, template):
        message = []
        tail = ''
        for element in template:
            if element[0] is '*': # Needs to be replaced
                if element[-1] is '.': # maintain punctuation
                    ary = '_uboat_' + element[1:-1]
                    tail = element[-1]
                else:
                    ary = '_uboat_' + element[1:]

                chosen = random.choice(getattr(self, ary))
                if type(chosen) is type([]):
                    message.append(' '.join(self._parse_template(chosen)) + tail)
                else:
                    message.append(chosen + tail)
            else:
                message.append(element)
        return message


    def death_message(self):
        """Returns a u-boat-death message.
        Returns a u-boat-death message patterned after messages in Iron
        Coffins."""
        template = random.choice(self._uboat_message)
        message = self._parse_template(template)
        message.append('U-' + str(random.randint(100, 999)) + '.')
        return ' '.join(message)


if __name__ == '__main__':
    print Uboat().death_message()
