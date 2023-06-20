import React from "react";
import "./ButtonLink.css";

interface AButtonProps {
  href: string;
  children: React.ReactNode;
  className?: string;
  onClick?: () => void;
}

const ButtonLink: React.FC<AButtonProps> = ({
  href,
  children,
  className,
  onClick,
}) => {
  return (
    <a
      href={href}
      className={`button-link${" " + className ?? ""}`}
      target="_blank"
      rel="noopener noreferrer"
      onClick={onClick}
    >
      {children}
    </a>
  );
};

interface ButtonProps {
  onClick?: () => void;
  children: React.ReactNode;
  className?: string;
}

const Button: React.FC<ButtonProps> = ({ onClick, children, className }) => {
  return (
    <button className={`button${" " + className ?? ""}`} onClick={onClick}>
      {children}
    </button>
  );
};

export { ButtonLink, Button };
