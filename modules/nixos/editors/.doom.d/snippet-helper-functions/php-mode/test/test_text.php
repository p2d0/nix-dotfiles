<?php

use Models\IndividualClassModel;
use PHPUnit\Framework\TestCase;
use Tables\IndividualClassesTable;

class IndividualClassesTest extends TestCase
{
    public function setUp()
    {
        $this->table = new IndividualClassesTable();
    }

    public function tearDown()
    {
    }

    function test_test()
    {
        $this->assertTrue(true);
        // $table = new IndividualClassesTable();
        // $class = new IndividualClassModel();
        // $class->id_product = 1;
        // $class->id_cart = 2;
        // $table->saveClass($class);
        // $table->getQuantity($class->id_cart,$class->id_product);
    }
    function test_date()
    {
        // $class = new Indivi
    }
}
