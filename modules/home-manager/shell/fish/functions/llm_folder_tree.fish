function llm_folder_tree -d "Capture folder structure and text file contents"
    if test (count $argv) -eq 0
        echo "Usage: llm_folder_tree <folder_path>"
        return 1
    end
    
    set temp_file (mktemp /tmp/llm_folder_tree_XXXXXX.txt)
    
    # Directory structure
    echo "=== Directory Structure ===" > $temp_file
    echo "" >> $temp_file
    tree "$argv[1]" -I 'node_modules|__pycache__|.git|*.pyc|*.o|*.class' >> $temp_file 2>/dev/null || find "$argv[1]" -type f | head -50 >> $temp_file
    echo "" >> $temp_file
    echo "=== File Contents ===" >> $temp_file
    echo "" >> $temp_file
    
    # File contents (limited to first 50 lines each to avoid huge output)
    for file in (find "$argv[1]" -type f | grep -E '\.(txt|md|py|js|java|cpp|c|h|html|css|json|yml|yaml|sh|scss)$' | head -20)
        if test -f "$file" && file "$file" | grep -q "text"
            echo "--- $file ---" >> $temp_file
            echo "" >> $temp_file
            head -n 50 -- "$file" >> $temp_file
            echo "" >> $temp_file
            echo "" >> $temp_file
        end
    end
    
    clip-file $temp_file
    echo "Folder structure and contents copied: $temp_file"
end


