
<?php

if (!defined('_PS_VERSION_')) {
    exit;
}

/**
 * Upgrade the Ps_Customtext module to V__VERSION__
 *
 * @param appointments_module $module
 *
 * @return bool
 */
function upgrade_module___(s-replace "." "_" (skeletor-sub "__VERSION__" subs))__($module)
{
    /* Db::getInstance()->execute(""); */
}
