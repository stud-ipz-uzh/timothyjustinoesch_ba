<?php

// Get all files in the __DIR__ . "/tags_stripped" folder
$files = glob(__DIR__ . "/tags_stripped/*.csv");

// Create the file H3_prepared_for_babel_machine.csv in the __DIR__ . "/prepared_for_babel" folder
$handle = fopen(__DIR__ . "/prepared_for_babel/H3_prepared_for_babel_machine.csv", "w");
fputcsv($handle, ['id', 'text', 'filename']);
fclose($handle);

foreach($files as $file) {
    prepare_for_bable_machine($file);
}

/**
 * Prepare the content column of a csv file for the Bable Machine
 */
function prepare_for_bable_machine($file) {
    // Open the file
    $handle = fopen($file, "r");

    // Read the first row
    $headers = fgetcsv($handle);

    // Read the rest into array
    $data = [];
    while (($row = fgetcsv($handle)) !== false) {
        $row_with_keys = array_combine($headers, $row);
        $data[] = $row_with_keys;
    }

    // Pluck the columns id and content from the data
    $prepared_data = array_map(function($row) {
        return [
            'id' => $row['id'],
            'text' => $row['content']
        ];
    }, $data);

    // Append a columne with the filename
    $filename = basename($file, '.csv');
    $prepared_data = array_map(function($row) use ($filename) {
        $row['filename'] = $filename;
        return $row;
    }, $prepared_data);

    // Append the file H3_prepared_for_babel_machine.csv in the __DIR__ . "/prepared_for_babel" folder
    $handle = fopen(__DIR__ . "/prepared_for_babel/H3_prepared_for_babel_machine.csv", "a");
    foreach ($prepared_data as $row) {
        fputcsv($handle, $row);
    }
    fclose($handle);
}