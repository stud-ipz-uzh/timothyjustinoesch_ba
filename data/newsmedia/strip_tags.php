<?php

// Get all .tsv files in the __DIR__ . "/raw" raw data folder
$files = glob(__DIR__ . "/raw/*.tsv");
var_dump($files);

foreach($files as $file) {
    strip_tags_from_tsv($file);
}

/**
 * Strip tags from the content column of a tsv file
 */
function strip_tags_from_tsv($file) {
    // Open the file
    $handle = fopen($file, "r");

    // Read the first row
    $headers = fgetcsv($handle, 0, "\t");

    // Read the rest into array
    $data = [];
    while (($row = fgetcsv($handle, 0, "\t")) !== false) {
        $row_with_keys = array_combine($headers, $row);
        $row_with_keys['content'] = strip_tags($row_with_keys['content']);
        $data[] = $row_with_keys;
    }

    // Write the data back to a new file in the tags_stripped folder
    $filename = basename($file, '.tsv');
    $handle = fopen(__DIR__ . "/tags_stripped/{$filename}.csv", "w");
    fputcsv($handle, $headers);
    foreach ($data as $row) {
        fputcsv($handle, $row);
    }
    fclose($handle);
}
