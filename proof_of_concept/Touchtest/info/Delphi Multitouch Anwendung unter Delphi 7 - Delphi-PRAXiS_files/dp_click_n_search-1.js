var searchprovider=new Array(3);searchprovider.google="http://www.google.de/search?meta=lr%3Dlang_de&q=";searchprovider.vbulletin="dp_search.php?"+SESSIONURL+"do=universal&search_keywords=";searchprovider.wikipedia="http://de.wikipedia.org/wiki/";function click_n_search(c,d){var a=searchprovider[d];if(a==undefined||a==""){alert("Interner Fehler: Ungültiger Provider.");return false}var b="";if(window.getSelection){b=new String(window.getSelection())}else{if(document.getSelection){b=new String(document.getSelection())}else{if(document.selection){b=new String(document.selection.createRange().text)}else{alert("Sorry - aber Dein Browser untersützt dieses Feature nicht.");return false}}}b=PHP.trim(b);if(b.length==0){alert("Click & Search - Unbekannte Begriffe einfach im Web finden.\nUnd so funktioniert es: Einen Begriff markieren und dann einen der Such-Links anklicken.\n\nAktuell ist kein Text markiert - daher kann auch keine Suche durchgeführt werden.");return false}else{if(b.length>100){alert("Es ist ist zuviel Text markiert.");return false}else{c.href=a+b;return true}}return false};