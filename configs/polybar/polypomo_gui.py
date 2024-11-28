#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sqlite3
from PyQt6.QtWidgets import QApplication, QMainWindow, QTableWidget, QTableWidgetItem, QVBoxLayout, QWidget, QPushButton, QLabel, QTabWidget, QGridLayout
from PyQt6.QtCore import Qt
from datetime import datetime, timedelta

def fetch_tasks(db_path, alltime=False, yesterday=False):
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()
    if alltime:
        cur.execute("""
            SELECT id, date, start, stop, title
            FROM sessions
            ORDER BY start
        """)
    elif yesterday:
        yesterday_date = (datetime.now() - timedelta(days=1)).strftime('%Y-%m-%d')
        cur.execute("""
            SELECT id, date, start, stop, title
            FROM sessions
            WHERE date(date) = ?
            ORDER BY start
        """, (yesterday_date,))
    else:
        cur.execute("""
            SELECT id, date, start, stop, title
            FROM sessions
            WHERE date(date) = date('now')
            ORDER BY start
        """)
    tasks = cur.fetchall()
    conn.close()
    return tasks

def fetch_tasks_this_month(db_path):
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()
    first_day_of_month = datetime.now().replace(day=1).strftime('%Y-%m-%d')
    last_day_of_month = (datetime.now().replace(day=1) + timedelta(days=31)).replace(day=1) - timedelta(days=1)
    last_day_of_month = last_day_of_month.strftime('%Y-%m-%d')
    cur.execute("""
        SELECT id, date, start, stop, title
        FROM sessions
        WHERE date(date) BETWEEN ? AND ?
        ORDER BY start
    """, (first_day_of_month, last_day_of_month))
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
        date, start, stop, title = task[1:5]
        duration = calculate_duration(start, stop)
        if title in task_durations:
            task_durations[title] += duration
        else:
            task_durations[title] = duration
    return task_durations

def aggregate_durations_alltime(tasks):
    return aggregate_durations(tasks)

def aggregate_durations_this_month(tasks):
    return aggregate_durations(tasks)

def update_task(db_path, task_id, new_start, new_stop, new_title):
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()
    cur.execute("UPDATE sessions SET start = ?, stop = ?, title = ? WHERE id = ?", (new_start, new_stop, new_title, task_id))
    conn.commit()
    conn.close()

class TasksWindow(QMainWindow):
    def __init__(self, tasks_today, task_durations_today, tasks_alltime, task_durations_alltime, tasks_yesterday, task_durations_yesterday, tasks_this_month, task_durations_this_month, db_path):
        super().__init__()

        self.setWindowTitle("Pomotasks")
        self.setGeometry(100, 100, 600, 400)
        self.db_path = db_path

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
        self.tasks_table.setColumnCount(7)  # Increased column count to 7
        self.tasks_table.setHorizontalHeaderLabels(["ID", "Date", "Start", "Stop", "Title", "Duration", "Pomodoros"])  # Added "Pomodoros" header
        self.tasks_table.setRowCount(len(tasks_today))

        # Populate the table with tasks
        for row, task in enumerate(tasks_today):
            task_id, date, start, stop, title = task
            duration = calculate_duration(start, stop)
            pomodoros = duration / 1500  # Calculate duration in pomodoros (1500 seconds = 25 minutes)
            self.tasks_table.setItem(row, 0, QTableWidgetItem(str(task_id)))
            self.tasks_table.setItem(row, 1, QTableWidgetItem(date))
            start_item = QTableWidgetItem(start)
            start_item.setFlags(start_item.flags() | Qt.ItemFlag.ItemIsEditable)  # Make the start cell editable
            self.tasks_table.setItem(row, 2, start_item)
            stop_item = QTableWidgetItem(stop)
            stop_item.setFlags(stop_item.flags() | Qt.ItemFlag.ItemIsEditable)  # Make the stop cell editable
            self.tasks_table.setItem(row, 3, stop_item)
            title_item = QTableWidgetItem(title)
            title_item.setFlags(title_item.flags() | Qt.ItemFlag.ItemIsEditable)  # Make the title cell editable
            self.tasks_table.setItem(row, 4, title_item)
            self.tasks_table.setItem(row, 5, QTableWidgetItem(str(datetime.utcfromtimestamp(duration).strftime('%H:%M:%S'))))  # Added duration column
            self.tasks_table.setItem(row, 6, QTableWidgetItem(f"{pomodoros:.2f}"))  # Added pomodoros column

        # Connect the itemChanged signal to the edit_task method
        self.tasks_table.itemChanged.connect(self.edit_task)

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
        self.durations_table.setColumnCount(3)
        self.durations_table.setHorizontalHeaderLabels(["Title", "Total Duration", "Pomodoros"])
        self.durations_table.setRowCount(len(task_durations_today) + 1)  # Add an extra row for the total

        # Populate the table with durations
        self.populate_durations_table(self.durations_table, task_durations_today)

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
        self.durations_alltime_table.setColumnCount(3)
        self.durations_alltime_table.setHorizontalHeaderLabels(["Title", "Total Duration", "Pomodoros"])
        self.durations_alltime_table.setRowCount(len(task_durations_alltime) + 1)  # Add an extra row for the total

        # Populate the table with durations
        self.populate_durations_table(self.durations_alltime_table, task_durations_alltime)

        durations_alltime_layout.addWidget(self.durations_alltime_table)

        # Tab for individual tasks yesterday
        tasks_yesterday_tab = QWidget()
        tab_widget.addTab(tasks_yesterday_tab, "Tasks Yesterday")

        tasks_yesterday_layout = QVBoxLayout()
        tasks_yesterday_tab.setLayout(tasks_yesterday_layout)

        # Add a label for tasks
        tasks_yesterday_label = QLabel("Tasks completed yesterday:")
        tasks_yesterday_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        tasks_yesterday_layout.addWidget(tasks_yesterday_label)

        # Create a table widget for tasks
        self.tasks_yesterday_table = QTableWidget()
        self.tasks_yesterday_table.setColumnCount(7)  # Increased column count to 7
        self.tasks_yesterday_table.setHorizontalHeaderLabels(["ID", "Date", "Start", "Stop", "Title", "Duration", "Pomodoros"])  # Added "Pomodoros" header
        self.tasks_yesterday_table.setRowCount(len(tasks_yesterday))

        # Populate the table with tasks
        for row, task in enumerate(tasks_yesterday):
            task_id, date, start, stop, title = task
            duration = calculate_duration(start, stop)
            pomodoros = duration / 1500  # Calculate duration in pomodoros (1500 seconds = 25 minutes)
            self.tasks_yesterday_table.setItem(row, 0, QTableWidgetItem(str(task_id)))
            self.tasks_yesterday_table.setItem(row, 1, QTableWidgetItem(date))
            start_item = QTableWidgetItem(start)
            start_item.setFlags(start_item.flags() | Qt.ItemFlag.ItemIsEditable)  # Make the start cell editable
            self.tasks_yesterday_table.setItem(row, 2, start_item)
            stop_item = QTableWidgetItem(stop)
            stop_item.setFlags(stop_item.flags() | Qt.ItemFlag.ItemIsEditable)  # Make the stop cell editable
            self.tasks_yesterday_table.setItem(row, 3, stop_item)
            title_item = QTableWidgetItem(title)
            title_item.setFlags(title_item.flags() | Qt.ItemFlag.ItemIsEditable)  # Make the title cell editable
            self.tasks_yesterday_table.setItem(row, 4, title_item)
            self.tasks_yesterday_table.setItem(row, 5, QTableWidgetItem(str(datetime.utcfromtimestamp(duration).strftime('%H:%M:%S'))))  # Added duration column
            self.tasks_yesterday_table.setItem(row, 6, QTableWidgetItem(f"{pomodoros:.2f}"))  # Added pomodoros column

        # Connect the itemChanged signal to the edit_task method
        self.tasks_yesterday_table.itemChanged.connect(self.edit_task)

        tasks_yesterday_layout.addWidget(self.tasks_yesterday_table)

        # Tab for aggregated durations yesterday
        durations_yesterday_tab = QWidget()
        tab_widget.addTab(durations_yesterday_tab, "Total Durations Yesterday")

        durations_yesterday_layout = QVBoxLayout()
        durations_yesterday_tab.setLayout(durations_yesterday_layout)

        # Add a label for durations
        durations_yesterday_label = QLabel("Total Duration by Task Title Yesterday:")
        durations_yesterday_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        durations_yesterday_layout.addWidget(durations_yesterday_label)

        # Create a table widget for durations
        self.durations_yesterday_table = QTableWidget()
        self.durations_yesterday_table.setColumnCount(3)
        self.durations_yesterday_table.setHorizontalHeaderLabels(["Title", "Total Duration", "Pomodoros"])
        self.durations_yesterday_table.setRowCount(len(task_durations_yesterday) + 1)  # Add an extra row for the total

        # Populate the table with durations
        self.populate_durations_table(self.durations_yesterday_table, task_durations_yesterday)

        durations_yesterday_layout.addWidget(self.durations_yesterday_table)

        # Tab for aggregated durations this month
        durations_this_month_tab = QWidget()
        tab_widget.addTab(durations_this_month_tab, "Total Durations This Month")

        durations_this_month_layout = QVBoxLayout()
        durations_this_month_tab.setLayout(durations_this_month_layout)

        # Add a label for durations
        durations_this_month_label = QLabel("Total Duration by Task Title This Month:")
        durations_this_month_label.setAlignment(Qt.AlignmentFlag.AlignCenter)
        durations_this_month_layout.addWidget(durations_this_month_label)

        # Create a table widget for durations
        self.durations_this_month_table = QTableWidget()
        self.durations_this_month_table.setColumnCount(3)
        self.durations_this_month_table.setHorizontalHeaderLabels(["Title", "Total Duration", "Pomodoros"])
        self.durations_this_month_table.setRowCount(len(task_durations_this_month) + 1)  # Add an extra row for the total

        # Populate the table with durations
        self.populate_durations_table(self.durations_this_month_table, task_durations_this_month)

        durations_this_month_layout.addWidget(self.durations_this_month_table)

        # Add a close button
        close_button = QPushButton("Close")
        close_button.clicked.connect(self.close)
        layout.addWidget(close_button)

    def populate_durations_table(self, table, task_durations):
        total_duration = 0
        row = 0
        for title, duration in task_durations.items():
            pomodoros = duration / 1500  # Calculate duration in pomodoros
            table.setItem(row, 0, QTableWidgetItem(title))
            table.setItem(row, 1, QTableWidgetItem(str(datetime.utcfromtimestamp(duration).strftime('%H:%M:%S'))))
            table.setItem(row, 2, QTableWidgetItem(f"{pomodoros:.2f}"))  # Added pomodoros column
            total_duration += duration
            row += 1

        # Add a total row
        total_pomodoros = total_duration / 1500
        table.setItem(row, 0, QTableWidgetItem("Total"))
        table.setItem(row, 1, QTableWidgetItem(str(datetime.utcfromtimestamp(total_duration).strftime('%H:%M:%S'))))
        table.setItem(row, 2, QTableWidgetItem(f"{total_pomodoros:.2f}"))

    def edit_task(self, item):
        row = item.row()
        task_id = int(self.tasks_table.item(row, 0).text())
        new_start = self.tasks_table.item(row, 2).text()
        new_stop = self.tasks_table.item(row, 3).text()
        new_title = self.tasks_table.item(row, 4).text()
        update_task(self.db_path, task_id, new_start, new_stop, new_title)
        # Refresh the durations table
        tasks_today = fetch_tasks(self.db_path, alltime=False)
        task_durations_today = aggregate_durations(tasks_today)
        self.update_durations_table(self.durations_table, task_durations_today)

    def update_durations_table(self, table, task_durations):
        self.populate_durations_table(table, task_durations)

def main():
    db_path = "/etc/nixos/configs/polybar/time.sqlite"
    tasks_today = fetch_tasks(db_path, alltime=False)
    task_durations_today = aggregate_durations(tasks_today)
    tasks_alltime = fetch_tasks(db_path, alltime=True)
    task_durations_alltime = aggregate_durations_alltime(tasks_alltime)
    tasks_yesterday = fetch_tasks(db_path, yesterday=True)
    task_durations_yesterday = aggregate_durations(tasks_yesterday)
    tasks_this_month = fetch_tasks_this_month(db_path)
    task_durations_this_month = aggregate_durations_this_month(tasks_this_month)

    app = QApplication([])
    window = TasksWindow(tasks_today, task_durations_today, tasks_alltime, task_durations_alltime, tasks_yesterday, task_durations_yesterday, tasks_this_month, task_durations_this_month, db_path)
    window.show()
    app.exec()

if __name__ == "__main__":
    main()