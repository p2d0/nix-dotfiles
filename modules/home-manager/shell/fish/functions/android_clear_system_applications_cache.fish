function android_clear_system_applications_cache
set packages (adb shell pm list packages -s | sed  's/package://')
for package in $packages
    echo $package
    adb shell pm clear $package
end
end
