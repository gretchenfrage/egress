package com.phoenixkahlo.hellcraft.carbonite;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Equivalent to @CarboniteWith(FieldNode.class)
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CarboniteFields {
}
