# chatgpt-emacs

A plugin for emacs that adds chatgpt support

# Usage

To use this plugin, you must define you chatgpt-api-key in your .emacs file.

```emacs-lisp
(setq chatgpt-api-key "YOUR_API_KEY")
```

To change what model you are using, update the chatgpt-model variable

```emacs-lisp
(setq chatgpt-model "gpt-4")
(setq chatgpt-model "gpt-3.5-turbo")
```

Then, you can use the command `chatgpt-reply` to start a chat with the bot.
