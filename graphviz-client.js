function mcDrawGraphviz(graphviz_source_element, 
			graphviz_server_path, 
			image_tag_element) {

    var renderer;
    if (window.XMLHttpRequest) {
        renderer = new XMLHttpRequest();
    } else {
        renderer = new ActiveXObject("Microsoft.XMLHTTP");
    }

    renderer.onreadystatechange = function () {
        if (renderer.readyState == 4) {
	    if (renderer.status == 200) {
		document.getElementById(image_tag_element).innerHTML =
                    renderer.responseText;
		document.getElementById(graphviz_source_element).innerText = 
		    renderer.responseText;
            }
	}
    }

    renderer.open("POST", graphviz_server_path, true);
    renderer.setRequestHeader('Content-Type', 'text/plain');
    renderer.send(document.getElementById(graphviz_source_element).innerText);
}
