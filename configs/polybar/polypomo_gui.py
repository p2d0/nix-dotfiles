#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sqlite3
from PyQt6.QtWidgets import QApplication, QMainWindow, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget, QPushButton, QLabel, QTabWidget, QGridLayout
from PyQt6.QtCore import Qt
from datetime import datetime

def fetch_tasks(db_path, alltime=False):
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()
    if alltime:
        cur.execute("""
            SELECT date, start, stop, title
            FROM sessions
            ORDER BY start
        """)
    else:
        cur.execute("""
            SELECT date, start, stop, title
            FROM sessions
            WHERE date(date) = date('now')
            ORDER BY start
        """)
    tasks = cur.fetchall()
    conn.close()
    return tasks

def calculate_duration(start, stop):
    start_time = datetime.strptime(start, "%H:%M:%S")
    stop_time = datetime.strptime(stop, "%H:%M:%S")
    duration = stop_time - start_time
    return duration.total_seconds()

def aggregate_durations(tasks):
    task_durations = {}
    for task in tasks:
        date, start, stop, title = task
        duration = calculate_duration(start, stop)
        if title in task_durations:
            task_durations[title] += duration
        else:
            task_durations[title] = duration
    return task_durations

def aggregate_durations_alltime(tasks):
    return aggregate_durations(tasks)

class TasksWindow(QMainWindow):
    def __init__(self, tasks_today, task_durations_today, tasks_alltime, task_durations_alltime):
        super().__init__()

        self.setWindowTitle("Pomotasks")
        self.setGeometry(100, 100, 600, 400)

        # Create a central widget and set the layout
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        layout = QVBoxLayout()
        central_widget.setLayout(layout)

        # Create a tab widget
        tab_widget = QTabWidget()
        layout.addWidget(tab_widget)

        # Tab for individual tasks today
        tasks_tab = QWidget()
        tab_widget.addTab(tasks_tab, "Tasks Today")

        tasks_layout = QVBoxLayout()
        tasks_tab.setLayout(tasks_layout)

        # Add a label for tasks
        tasks_label = QLabel("Tasks completed today:")
        tasks_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        tasks_layout.addWidget(tasks_label)

        # Create a table widget for tasks
        self.tasks_table = QTableWidget()
        self.tasks_table.setColumnCount(5)
        self.tasks_table.setHorizontalHeaderLabels(["Date", "Start", "Stop", "Title", "Duration"])
        self.tasks_table.setRowCount(len(tasks_today))

        # Populate the table with tasks
        for row, task in enumerate(tasks_today):
            date, start, stop, title = task
            duration = calculate_duration(start, stop)
            self.tasks_table.setItem(row, 0, QTableWidgetItem(date))
            self.tasks_table.setItem(row, 1, QTableWidgetItem(start))
            self.tasks_table.setItem(row, 2, QTableWidgetItem(stop))
            self.tasks_table.setItem(row, 3, QTableWidgetItem(title))
            self.tasks_table.setItem(row, 4, QTableWidgetItem(str(datetime.utcfromtimestamp(duration).strftime('%H:%M:%S'))))

        tasks_layout.addWidget(self.tasks_table)

        # Tab for aggregated durations today
        durations_tab = QWidget()
        tab_widget.addTab(durations_tab, "Total Durations Today")

        durations_layout = QVBoxLayout()
        durations_tab.setLayout(durations_layout)

        # Add a label for durations
        durations_label = QLabel("Total Duration by Task Title Today:")
        durations_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        durations_layout.addWidget(durations_label)

        # Create a table widget for durations
        self.durations_table = QTableWidget()
        self.durations_table.setColumnCount(2)
        self.durations_table.setHorizontalHeaderLabels(["Title", "Total Duration"])
        self.durations_table.setRowCount(len(task_durations_today))

        # Populate the table with durations
        row = 0
        for title, duration in task_durations_today.items():
            self.durations_table.setItem(row, 0, QTableWidgetItem(title))
            self.durations_table.setItem(row, 1, QTableWidgetItem(str(datetime.utcfromtimestamp(duration).strftime('%H:%M:%S'))))
            row += 1

        durations_layout.addWidget(self.durations_table)

        # Tab for aggregated durations alltime
        durations_alltime_tab = QWidget()
        tab_widget.addTab(durations_alltime_tab, "Total Durations Alltime")

        durations_alltime_layout = QVBoxLayout()
        durations_alltime_tab.setLayout(durations_alltime_layout)

        # Add a label for durations
        durations_alltime_label = QLabel("Total Duration by Task Title Alltime:")
        durations_alltime_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        durations_alltime_layout.addWidget(durations_alltime_label)

        # Create a table widget for durations
        self.durations_alltime_table = QTableWidget()
        self.durations_alltime_table.setColumnCount(2)
        self.durations_alltime_table.setHorizontalHeaderLabels(["Title", "Total Duration"])
        self.durations_alltime_table.setRowCount(len(task_durations_alltime))

        # Populate the table with durations
        row = 0
        for title, duration in task_durations_alltime.items():
            self.durations_alltime_table.setItem(row, 0, QTableWidgetItem(title))
            self.durations_alltime_table.setItem(row, 1, QTableWidgetItem(str(datetime.utcfromtimestamp(duration).strftime('%H:%M:%S'))))
            row += 1

        durations_alltime_layout.addWidget(self.durations_alltime_table)

        # Add a close button
        close_button = QPushButton("Close")
        close_button.clicked.connect(self.close)
        layout.addWidget(close_button)

def main():
    db_path = "/etc/nixos/configs/polybar/time.sqlite"
    tasks_today = fetch_tasks(db_path, alltime=False)
    task_durations_today = aggregate_durations(tasks_today)
    tasks_alltime = fetch_tasks(db_path, alltime=True)
    task_durations_alltime = aggregate_durations_alltime(tasks_alltime)

    app = QApplication([])
    window = TasksWindow(tasks_today, task_durations_today, tasks_alltime, task_durations_alltime)
    window.show()
    app.exec()

if __name__ == "__main__":
    main()