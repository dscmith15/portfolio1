<?php
// this writes data to local server storage on the pi
$filename = "dataP3/".$_POST['filename'];
$data = $_POST['filedata'];
//write the file to the disk
file_put_contents($filename, $data);
?>
