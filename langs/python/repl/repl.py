import sys
from PyQt5 import QtCore, QtWidgets, QtGui, Qt
from PyQt5.Qsci import QsciScintilla, QsciLexerPython
import QTermWidget

# forwards trick


def get_chunk_lines(ed):
    return _get_chunk_lines(ed)


class Editor(QsciScintilla):
    ARROW_MARKER_NUM = 8
    keyPressed = QtCore.pyqtSignal(QtCore.QEvent)
    sendCommand = QtCore.pyqtSignal(str)

    def __init__(self, parent=None):
        super(Editor, self).__init__(parent)

        # Set the default font
        font = QtGui.QFont("Monospace")
        font.setStyleHint(QtGui.QFont.TypeWriter)
        self.setFont(font)
        # font.setFamily('Courier')
        font.setFixedPitch(True)
        font.setPointSize(12)
        self.setFont(font)
        self.setMarginsFont(font)

        # Margin 0 is used for line numbers
        fontmetrics = QtGui.QFontMetrics(font)
        self.setMarginsFont(font)
        self.setMarginWidth(1, fontmetrics.width("0000") + 6)
        self.setMarginLineNumbers(1, True)
        self.setMarginsBackgroundColor(QtGui.QColor("#cccccc"))
        self.setIndentationWidth(4)

        # Clickable margin 1 for showing markers
        # self.setMarginSensitivity(1, True)
        # self.marginClicked.connect(self.on_margin_clicked)
        # self.markerDefine(QsciScintilla.RightArrow,
        #     self.ARROW_MARKER_NUM)
        # self.setMarkerBackgroundColor(QtGui.QColor("#ee1111"),
        #     self.ARROW_MARKER_NUM)

        # Brace matching: enable for a brace immediately before or after
        # the current position
        #
        self.setBraceMatching(QsciScintilla.SloppyBraceMatch)

        # Current line visible with special background color
        self.setCaretLineVisible(True)
        self.setCaretLineBackgroundColor(QtGui.QColor("#ffe4e4"))

        # Set Python lexer
        # Set style for Python comments (style number 1) to a fixed-width
        # courier.
        #

        # lexer = QsciLexerPython()
        # # lexer.setDefaultFont(font)
        # lexer.setFont(font)
        # self.setLexer(lexer)

        # text = bytearray(str.encode("Monospace"))
        # 32, "Courier New"
        #self.SendScintilla(QsciScintilla.SCI_STYLESETFONT, 1, text)

        # Don't want to see the horizontal scrollbar at all
        # Use raw message to Scintilla here (all messages are documented
        # here: http://www.scintilla.org/ScintillaDoc.html)
        # self.SendScintilla(QsciScintilla.SCI_SETHSCROLLBAR, 0)
        self.SendScintilla(QsciScintilla.SCI_SETSCROLLWIDTHTRACKING, 1)

        # not too small
        # self.setMinimumSize(600, 450)

    def resizeEvent(self, event: QtGui.QResizeEvent):
        self.SendScintilla(QsciScintilla.SCI_SETSCROLLWIDTH, event.size().width() - self.marginWidth(1)- self.marginWidth(0))
        return super(Editor, self).resizeEvent(event)
    # def on_margin_clicked(self, nmargin, nline, modifiers):
    #     # Toggle marker for the line the margin was clicked on
    #     if self.markersAtLine(nline) != 0:
    #         self.markerDelete(nline, self.ARROW_MARKER_NUM)
    #     else:
    #         self.markerAdd(nline, self.ARROW_MARKER_NUM)

    def keyPressEvent(self, event):
        if event.key() == QtCore.Qt.Key_Enter or event.key() == QtCore.Qt.Key_Return:
            current_line = self.text(self.getCursorPosition()[0])
            if(current_line[-1] == '\n'):
                current_line = current_line[:-1]
            (r, c) = self.getCursorPosition()
            if (bool(event.modifiers() & QtCore.Qt.ShiftModifier)):
                for line in get_chunk_lines(self):
                    self.sendCommand.emit(line + '\n')
            elif (bool(event.modifiers() & QtCore.Qt.ControlModifier)):
                self.insertAt("\n", r, c)
                self.setCursorPosition(r+1, 0)
                super(Editor, self).keyPressEvent(event)
            elif(self.getCursorPosition()[1] == len(current_line)):
                self.sendCommand.emit(current_line)
                super(Editor, self).keyPressEvent(event)
            elif(self.getCursorPosition()[1] == 0):
                self.sendCommand.emit(current_line)
                self.setCursorPosition(r+1, 0)
            elif(self.getCursorPosition()[1] != len(current_line)):
                super(Editor, self).keyPressEvent(event)
        else:
            super(Editor, self).keyPressEvent(event)


def _get_chunk_lines(ed):
    def get_lines_till_empty(ed, rang):
        lines = []
        (cr, _) = ed.getCursorPosition()
        for r in rang:
            line = ed.text(r).strip()
            if(line == ''):
                break
            else:
                lines.append(line)
        return lines

    (cr, _) = ed.getCursorPosition()
    return get_lines_till_empty(ed, range(cr, -1, -1))[::-1] + get_lines_till_empty(ed, range(cr+1, ed.lines()))


class MainWindow(QtWidgets.QWidget):
    def __init__(self):
        QtWidgets.QMainWindow.__init__(self)
        self.layout = QtWidgets.QVBoxLayout()
        self._splitter = QtWidgets.QSplitter(self)
        self._splitter.setOrientation(QtCore.Qt.Horizontal)
        self.layout.addWidget(self._splitter)
        self.setLayout(self.layout)
        self._editor = Editor(self._splitter)
        self._term = QTermWidget.QTermWidget(1, self._splitter)
        # print(self._term.availableColorSchemes())
        self._term.setColorScheme('DarkPastels')

        self._splitter.setSizes([1000, 1000])
        self._editor.setFocus()
        self._editor.sendCommand.connect(self.on_send_command)

    @QtCore.pyqtSlot(str)
    def on_send_command(self, command):
        self._term.sendText(command + '\n')


if __name__ == '__main__':
    app = QtWidgets.QApplication(sys.argv)
    win = MainWindow()
    win.show()
    sys.exit(app.exec_())
