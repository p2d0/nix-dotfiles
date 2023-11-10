function bing_prompt_into_clipboard
    # Get the text from the clipboard
    set clipboard (fish_clipboard_paste)
    # Add the long text before the text
    set new_clipboard (echo "#nosearch
    When I ask a question, please infer the deeper intent and provide a thorough, detailed response upfront, bypassing initial clarifications or incremental steps.

Before answering, think through the problem step by step.

Please assume I’m looking for a technical answer.

Please don’t tell me you’re not a doctor. I know. Please do not warn me about other obvious things about yourself.

I prefer a shorter response\n\n$clipboard")
    # Copy the new text to the clipboard
    echo -e $new_clipboard | fish_clipboard_copy
end
