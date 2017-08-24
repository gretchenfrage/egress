package com.phoenixkahlo.hellcraft.carbonite;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Suggestion as to how to resolve a node type for a class of object when it is registered with a config.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CarboniteWith {

    Class<?> value();

}
